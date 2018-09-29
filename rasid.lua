local sched = require('sched')
local net = require('net')
local sdl = require('sdl2')
local audio = require('audio')
local sndfile = require('sndfile')
local fluid = require('fluidsynth')
local util = require('util')
local stream = require('stream')
local fs = require('fs')

local M = {}

local R = {
  SAMPLE_RATE = 48000,
  BPM = 120,
  VEL = 96,
  ROOT = 60,
}

function R.b2s(beats)
   local beats_per_second = R.BPM / 60
   local seconds_per_beat = 1 / beats_per_second
   return beats * seconds_per_beat
end

function R.wait(beats)
   return sched.sleep(R.b2s(beats))
end

function R.sleep(seconds)
   return sched.sleep(seconds)
end

note_offsets = {
   c = 0,
   d = 2,
   e = 4,
   f = 5,
   g = 7,
   a = 9,
   b = 11,
}

function R.note(note)
   if type(note) == "string" then
      local c4 = 60
      local offset = 0
      for i=1,#note do
         local ch = string.lower(note:sub(i,i))
         if note_offsets[ch] then
            offset = note_offsets[ch]
         elseif ch >= '0' and ch <= '9' then
            offset = offset + (tonumber(ch)-4)*12
         elseif ch == '-' then
            offset = offset - 1
         elseif ch == '+' then
            offset = offset + 1
         end
      end
      note = c4 + offset
   end
   return note
end

local function iter(x)
   if type(x) == "table" then
      if type(x.iter) == "function" then
         return x:iter()
      else
         local i = 1
         return function()
            local rv
            if i <= #x then
               rv = x[i]
               i = i + 1
            else
               rv = nil
            end
            return rv
         end
      end
   elseif type(x) == "function" then
      return x
   else
      return function() return x end
   end
end

function R.loop(x)
   local i = iter(x)
   local values = {}
   local consumed = false
   return function()
      local next_val = i()
      if next_val == nil then
         i = iter(values)
         next_val = i()
         consumed = true
      end
      if not consumed then
         table.insert(values, next_val)
      end
      return next_val
   end
end

local Scale = util.Class()

function Scale:create(steps)
   local self = {
      offsets = {},
   }
   local offset = 0
   for i=1,#steps do
      table.insert(self.offsets, offset)
      offset = offset + steps[i]
   end
   return self
end

function Scale:at(degree)
   local index = degree % #self.offsets
   local octave_shift = math.floor(degree / #self.offsets)
   return self.offsets[index+1] + octave_shift * 12
end

function Scale:iter()
   return iter(self.offsets)
end

R.Scale = Scale

R.scales = {
   major = Scale { 2,2,1,2,2,2,1 },
   minor = Scale { 2,1,2,2,1,2,2 },
}

local Pattern = util.Class()

function Pattern:iter()
   local iters = {}
   for k,v in pairs(self) do
      if k ~= "template" then
         iters[k] = iter(v)
      end
   end
   return function()
      local e = {}
      for k,i in pairs(iters) do
         e[k] = i()
      end
      return self.template:extend(e)
   end
end

R.Pattern = Pattern

local Event = util.Class()

function Event:create(opts)
   opts = opts or {}
   opts.note = opts.note and R.note(opts.note)
   opts.root = opts.root and R.note(opts.root)
   return opts
end

function Event:play(channel)
   local note, root, scale, degree, shift, transpose
   note = self.note
   if not note then
      root = self.root or R.ROOT
      scale = self.scale or R.scales.major
      degree = self.degree or 0
      shift = self.shift or 0
      transpose = self.transpose or 0
      if type(degree) == "table" then
         note = {}
         for i=1,#degree do
            table.insert(note, root + scale:at(degree[i] + shift) + transpose)
         end
      else
         note = root + scale:at(degree + shift) + transpose
      end
   end
   dur = self.dur
   vel = self.vel or R.VEL
   sched(function()
       if type(note) == "table" then
          for i=1,#note do
             channel:noteon(note[i], vel)
          end
       else
          channel:noteon(note, vel)
       end
       if dur then
          R.wait(dur)
          if type(note) == "table" then
             for i=1,#note do
                channel:noteoff(note[i])
             end
          else
             channel:noteoff(note)
          end
       end
   end)
end

function Event:clone()
   local opts = {}
   for k,v in pairs(self) do
      opts[k] = v
   end
   return Event(opts)
end

function Event:extend(opts)
   local e = self:clone()
   for k,v in pairs(opts) do
      e[k] = v
   end
   return e
end

R.Event = Event

local function parse_ticks(ticks)
   if type(ticks) == "string" then
      local rv = {}
      for i=1,#ticks do
         local ch = ticks:sub(i,i)
         if ch == '.' then
            table.insert(rv, false)
         else
            table.insert(rv, true)
         end
      end
      return rv
   else
      return ticks
   end
end

function R.play(opts)
   local i_tick = iter(parse_ticks(opts.ticks))
   local i_event = iter(opts.events)
   sched(function()
      while true do
         local tick = i_tick()
         if tick == nil then
            break
         elseif tick then
            local event = i_event()
            event:play(opts.channel)
         end
         R.wait(opts.bpt or 1)
      end
   end)
end

local mixer = audio.Mixer()

local SoundFont = util.Class()

function SoundFont:create(synth, sfont_id)
   return {
      synth = synth,
      sfont_id = sfont_id,
   }
end

local FluidSynth = util.Class()

function FluidSynth:create(settings)
   local self = {}
   if not settings then
      settings = fluid.Settings()
      settings:setnum("synth.gain", 1)
      settings:setint("synth.midi-channels", 256)
      settings:setnum("synth.sample-rate", R.SAMPLE_RATE)
   end
   local synth = fluid.Synth(settings)
   local source = fluid.AudioSource(synth)
   mixer:add(source)
   sched.on('quit', function()
      synth:delete()
      settings:delete()
   end)
   self.synth = synth
   return self
end

function FluidSynth:sfload(...)
   local sfont_id = self.synth:sfload(...)
   return SoundFont(self, sfont_id)
end

function FluidSynth:channel(chan)
   local synth = self.synth
   local channel = {}
   function channel:sfont(sfont)
      synth:sfont_select(chan, sfont.sfont_id)
      return self
   end
   function channel:bank(bank)
      synth:bank_select(chan, bank)
      return self
   end
   function channel:program(program)
      synth:program_change(chan, program)
      return self
   end
   function channel:noteon(key, vel)
      synth:noteon(chan, R.note(key), vel or R.VEL)
      return self
   end
   function channel:noteoff(key)
      synth:noteoff(chan, R.note(key))
      return self
   end
   local function defproxy(name)
      channel[name] = function(self, ...)
         synth[name](synth, chan, ...)
         return self
      end
   end
   defproxy("cc")
   defproxy("pitch_bend")
   defproxy("pitch_wheel_sens")
   defproxy("channel_pressure")
   defproxy("all_notes_off")
   defproxy("all_sounds_off")
   return channel
end

R.FluidSynth = FluidSynth

function M.main()
   local device = audio.Device {
      freq = R.SAMPLE_RATE,
      channels = 2,
      samples = 256,
      source = mixer,
   }
   device:start()
   sched.on('quit', function()
      device:stop()
      device:close()
   end)
   local stdio = stream.duplex(fs.fd(0), fs.fd(1))
   stdio:write("===[ Rasid v1.0.0 ]===\n")
   stdio:write("Send me blocks of Lua code for evaluation.\n")
   stdio:write("Each block should end with an empty comment line (\"\\n--\\n\")\n")
   local function perr(err)
      stdio:write(tostring(err))
      stdio:write("\n")
   end
   local env = setmetatable({ R = R }, { __index = _G })
   while not stdio:eof() do
      stdio:write("> ")
      local buf = stdio:read_until("\n--\n")
      local chunk, err = loadstring(tostring(buf))
      if not chunk then
         perr(err)
      else
         sched(function()
            setfenv(0, env)
            local ok, err = pcall(chunk)
            if not ok then
               perr(err)
            end
         end)
      end
   end
   stdio:write("\n")
end

return M

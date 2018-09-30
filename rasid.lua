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

local function ValueIterator(value, n)
   local self = { is_iter = true }
   if n then
      function self:at()
         if n > 0 then
            n = n - 1
            return value
         else
            return nil
         end
      end
   else
      function self:at()
         return value
      end
   end
   return self
end

local function ArrayIterator(array, n)
   local self = { is_iter = true }
   local max_index = n and (n * #array)
   function self:at(index)
      if max_index and index > max_index then
         return nil
      else
         index = ((index - 1) % #array) + 1
         return array[index]
      end
   end
   return self
end

local function is_iter(x)
   return type(x) == "table" and x.is_iter
end

local function iter(x, n)
   -- n == nil: repeat forever, n > 0: repeat N times
   if type(x) == "table" then
      if type(x.iter) == "function" then
         return x:iter(n)
      else
         return ArrayIterator(x, n)
      end
   elseif is_iter(x) then
      return x
   else
      return ValueIterator(x, n)
   end
end

local Iterable = util.Class()

function Iterable:iter(n)
   return ValueIterator(self, n)
end

local Scale = util.Class(Iterable)

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

R.Scale = Scale

R.scales = {
   major = Scale { 2,2,1,2,2,2,1 },
   minor = Scale { 2,1,2,2,1,2,2 },
}

local Chord = util.Class(Iterable)

R.Chord = Chord

local Event = util.Class(Iterable)

function Event:create(opts)
   opts = opts or {}
   opts.root = opts.root and R.note(opts.root)
   return opts
end

function Event:play()
   local synth, sfont, channel, bank, program
   local root, scale, degree, shift, transpose, chord, dur, vel
   synth = self.synth
   sfont = self.sfont
   channel = self.channel or 0
   bank = self.bank
   program = self.program
   root = self.root or R.ROOT
   scale = self.scale or R.scales.major
   degree = self.degree or 0
   shift = self.shift or 0
   transpose = self.transpose or 0
   chord = self.chord
   dur = self.dur
   vel = self.vel or R.VEL
   local notes = {}
   if chord then
      for i=1,#chord do
         local chord_shift = chord[i]
         local scale_index = degree + shift + chord_shift
         table.insert(notes, root + scale:at(scale_index) + transpose)
      end
   else
      local scale_index = degree + shift
      table.insert(notes, root + scale:at(scale_index) + transpose)
   end
   sched(function()
       if sfont then
          synth:sfont(channel, sfont)
       end
       if bank then
          synth:bank(channel, bank)
       end
       if program then
          synth:program(channel, program)
       end
       for i=1,#notes do
          synth:noteon(channel, notes[i], vel)
       end
       if dur then
          R.wait(dur)
          for i=1,#notes do
             synth:noteoff(channel, notes[i])
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

local Pattern = util.Class()

function Pattern:create(opts)
   local self = {
      is_pattern = true,
      ticks = parse_ticks(opts.ticks),
      rep = opts.rep or 1,
      bpt = opts.bpt or 1,
   }
   self.iters = {}
   if opts.events then
      for k,v in pairs(opts.events) do
         self.iters[k] = iter(v)
      end
   end
   self.length = opts.length or self.rep * #self.ticks
   return self
end

function Pattern:set(k, v)
   self.iters[k] = iter(v)
end

local function EventIterator(iters)
   local self = { is_iter = true }
   function self:at(index)
      local opts = {}
      for k,i in pairs(iters) do
         opts[k] = i:at(index)
      end
      if opts.template then
         return opts.template:extend(opts)
      else
         return Event(opts)
      end
   end
   return self
end

function Pattern:play()
   local length = self.length
   local i_tick = iter(self.ticks)
   local i_bpt = iter(self.bpt)
   local i_event = EventIterator(self.iters)
   local event_index = 0
   self.playing = true
   for i=1,length do
      if not self.playing then
         break
      end
      if i_tick:at(i) then
         event_index = event_index + 1
         local event = i_event:at(event_index)
         event:play()
      end
      R.wait(i_bpt:at(i))
   end
end

function Pattern:stop()
   self.playing = false
end

R.Pattern = Pattern

local function is_pattern(x)
   return type(x) == "table" and x.is_pattern
end

function R.play(pattern)
   assert(is_pattern(pattern))
   sched(function()
     pattern:play()
   end)
end

function R.play_forever(pattern)
   assert(is_pattern(pattern))
   sched(function()
     pattern:play()
     while pattern.playing do
        pattern:play()
     end
   end)
end

function R.stop(pattern)
   assert(is_pattern(pattern))
   pattern:stop()
end

local mixer = audio.Mixer()

local SoundFont = util.Class(Iterable)

function SoundFont:create(synth, sfont_id)
   return {
      synth = synth,
      sfont_id = sfont_id,
   }
end

local FluidSynth = util.Class(Iterable)

function FluidSynth:create(settings)
   local self = {}
   if not settings then
      settings = fluid.Settings()
      settings:setnum("synth.gain", 1)
      settings:setint("synth.midi-channels", 1024)
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

function FluidSynth:sfont(chan, sfont)
   self.synth:sfont_select(chan, sfont.sfont_id)
   return self
end

function FluidSynth:bank(chan, bank)
   self.synth:bank_select(chan, bank)
   return self
end

function FluidSynth:program(chan, program)
   self.synth:program_change(chan, program)
   return self
end

function FluidSynth:noteon(chan, key, vel)
   self.synth:noteon(chan, R.note(key), vel or R.VEL)
   return self
end

function FluidSynth:noteoff(chan, key)
   self.synth:noteoff(chan, R.note(key))
   return self
end

do
   local function defproxy(name)
      FluidSynth[name] = function(self, ...)
         self.synth[name](self.synth, ...)
         return self
      end
   end
   defproxy("cc")
   defproxy("pitch_bend")
   defproxy("pitch_wheel_sens")
   defproxy("channel_pressure")
   defproxy("all_notes_off")
   defproxy("all_sounds_off")
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

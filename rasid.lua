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
  sample_rate = 48000,
  bpm = 120,
  vel = 96,
  root = 60,
}

function R.nop()
   -- do nothing
end

function R.b2s(beats, bpm)
   local beats_per_second = (bpm or R.bpm) / 60
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

local function parse_note(note)
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
      function self:at(index)
         if index <= n then
            return value
         else
            return nil
         end
      end
   else
      function self:at(index)
         return value
      end
   end
   return self
end

local function ArrayIterator(array, n)
   local self = { is_iter = true }
   local max_index = n and (n * #array)
   function self:at(index, wrap)
      if max_index and index > max_index then
         return nil
      elseif wrap then
         index = ((index - 1) % #array) + 1
      end
      return array[index]
   end
   return self
end

local function is_iter(x)
   return type(x) == "table" and x.is_iter
end

local function iter(x, n)
   -- n == nil: repeat forever, n > 0: repeat N times
   if is_iter(x) then
      return x
   elseif type(x) == "table" then
      if type(x.iter) == "function" then
         return x:iter(n)
      else
         return ArrayIterator(x, n)
      end
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

local function play(event)
   local synth, sfont, channel, bank, program
   local root, scale, degree, shift, transpose, chord, dur, vel
   synth = event.synth
   sfont = event.sfont
   channel = event.channel or 0
   bank = event.bank
   program = event.program
   root = parse_note(event.root or R.root)
   scale = event.scale or R.scales.major
   degree = event.degree or 0
   shift = event.shift or 0
   transpose = event.transpose or 0
   chord = event.chord
   dur = event.dur
   vel = event.vel or R.vel
   local notes = {}
   if chord then
      for i=1,#chord do
         local chord_offset = chord[i]
         local scale_index = degree + shift + chord_offset
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

local function identity(x)
   return x
end

local function map(fn, t)
   fn = fn or identity
   local copy = {}
   for k,v in pairs(t) do
      copy[k] = fn(v)
   end
   return copy
end

function rpairs(t)
   local k,v
   local keys_seen = {}
   return function()
      while true do
         k,v = next(t,k)
         if k then
            if not keys_seen[k] then
               keys_seen[k] = true
               return k,v
            end
         else
            local mt = getmetatable(t)
            if mt and mt.__index then
               t = mt.__index
            else
               return nil
            end
         end
      end
   end
end

local next_playing_item

local Phrase = util.Class(Iterable)

function Phrase:create(opts)
   opts = opts or {}
   local self = {
      ticks = opts.ticks or "1",
      rep = opts.rep or 1,
      bpm = opts.bpm or R.bpm,
      bpt = opts.bpt or 1,
      events = map(iter, opts.events or {}),
   }
   self.i_tick = iter(parse_ticks(self.ticks))
   self.i_bpm = iter(self.bpm)
   self.i_bpt = iter(self.bpt)
   self.length = opts.length or #self.ticks
   next_playing_item = self
   return self
end

function Phrase:set(k, v)
   if k == "ticks" then
      self.ticks = v
      self.i_tick = iter(parse_ticks(v))
      self.length = #v
   elseif k == "rep" then
      self.rep = v
   elseif k == "bpm" then
      self.bpm = v
      self.i_bpm = iter(v)
   elseif k == "bpt" then
      self.bpt = v
      self.i_bpt = iter(v)
   else
      self.events[k] = iter(v)
   end
end

function Phrase:extend(opts)
   local p = Phrase {
      ticks = opts.ticks or self.ticks,
      rep = opts.rep or self.rep,
      bpm = opts.bpm or self.bpm,
      bpt = opts.bpt or self.bpt,
   }
   p.events = setmetatable({}, { __index = self.events })
   for k,v in pairs(opts) do
      p:set(k, v)
   end
   return p
end

local function EventIterator(events)
   local self = { is_iter = true }
   function self:at(index, wrap)
      local event = {}
      for k,i in rpairs(events) do
         event[k] = i:at(index, wrap)
      end
      return event
   end
   return self
end

function Phrase:play(forever)
   self.playing = true
   self.forever = forever
   local i_event = EventIterator(self.events)
   local event_index = 0
   local wrap = true
   while self.playing do
      local length = self.length * self.rep
      for i=1,length do
         if not self.playing then
            break
         end
         if self.i_tick:at(i, wrap) then
            event_index = event_index + 1
            local event = i_event:at(event_index, wrap)
            play(event)
         end
         local beats_per_tick = self.i_bpt:at(i, wrap)
         local bpm = self.i_bpm:at(i, wrap)
         local seconds_per_tick = R.b2s(beats_per_tick, bpm)
         R.sleep(seconds_per_tick)
      end
      if not forever then
         break
      end
   end
end

function Phrase:stop()
   self.playing = false
end

R.Phrase = Phrase

local Seq = util.Class(Iterable)

function Seq:create(tracks)
   local self = {
      tracks = map(iter, tracks or {}),
   }
   next_playing_item = self
   return self
end

function Seq:set(k, v)
   self.tracks[k] = iter(v)
end

function Seq:extend(opts)
   local s = Seq()
   s.tracks = setmetatable({}, { __index = self.tracks })
   for k,v in pairs(opts) do
      s:set(k, v)
   end
   return s
end

function Seq:play(forever)
   self.playing = true
   self.forever = forever
   local wrap = forever
   local env = getfenv(0)
   while self.playing do
      local threads = {}
      for track_name,i_track in rpairs(self.tracks) do
         local function play_track()
            local index = 1
            repeat
               local item = i_track:at(index, wrap)
               if item then
                  if type(item) == "number" then
                     local map = env[track_name]
                     item = map[item]
                  end
                  if item then
                     item:play()
                  end
               end
               index = index + 1
            until not self.playing or not item
         end
         table.insert(threads, sched(play_track))
      end
      -- wait until all tracks finish playing
      sched.join(threads)
      if not forever then
         break
      end
   end
end

function Seq:stop()
   self.playing = false
end

R.Seq = Seq

local current_playing_item

function R.play(x, forever)
   x = x or next_playing_item
   if current_playing_item and current_playing_item.forever then
      current_playing_item:stop()
   end
   current_playing_item = x
   sched(function()
      x:play(forever)
   end)
end

function R.play_forever(x)
   R.play(x, true)
end

function R.stop(x)
   x = x or current_playing_item
   x:stop()
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
      settings:setnum("synth.sample-rate", R.sample_rate)
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
   self.synth:noteon(chan, parse_note(key), vel or R.vel)
   return self
end

function FluidSynth:noteoff(chan, key)
   self.synth:noteoff(chan, parse_note(key))
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
      freq = R.sample_rate,
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
            setfenv(chunk, env)
            setfenv(0, env) -- ensure loadstring() preserves env
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

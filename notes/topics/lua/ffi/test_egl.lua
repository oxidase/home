--[[ Original source egl-example.cpp with a comment:
Created by exoticorn ( http://talk.maemo.org/showthread.php?t=37356 )
edited and commented by André Bergner [endboss]
--]]

local ffi = require "ffi"
local Xlib = require "xlib"
local EGL = require "egl"
local GLES2 = require "gles2"
local gl = GLES2.Lib;

-- the X11 part
-- in the first part the program opens a connection to the X11 window manager
x_display = Xlib.Display.new();

local swa = ffi.new("XSetWindowAttributes");
swa.event_mask = Xlib.ExposureMask + Xlib.PointerMotionMask + Xlib.KeyPressMask;
x_window = Xlib.Window.new(x_display, nil, 0, 0, 800, 480, 0,
   Xlib.CopyFromParent, Xlib.InputOutput, nil, Xlib.CWEventMask, swa);
x_window:XStoreName("GL test");

xev = ffi.new("XEvent");
xev.type                 = Xlib.ClientMessage;
xev.xclient.window       = x_window.handle;
xev.xclient.message_type = Xlib.Lib.XInternAtom(x_display.handle, "_NET_WM_STATE", false);
xev.xclient.format       = 32;
xev.xclient.data.l[0]    = 1;
xev.xclient.data.l[1]    = Xlib.Lib.XInternAtom(x_display.handle, "_NET_WM_STATE_FULLSCREEN", true);
Xlib.Lib.XSendEvent(x_display.handle, x_display:DefaultRootWindow(), false, Xlib.SubstructureNotifyMask, xev);
x_window:HideCursor()

-- the egl part
-- egl provides an interface to connect the graphics related functionality of openGL ES
-- with the windowing interface and functionality of the native operation system (X11) in our case.
local config_attr = ffi.new("EGLint[5]",
    EGL.EGL_BUFFER_SIZE, 16,
    EGL.EGL_RENDERABLE_TYPE,
    EGL.EGL_OPENGL_ES2_BIT,
    EGL.EGL_NONE);
local context_attr = ffi.new("EGLint[3]",
    EGL.EGL_CONTEXT_CLIENT_VERSION, 2,
    EGL.EGL_NONE);
local egl_display = EGL.Display.new(x_display.handle, EGL.EGL_OPENGL_ES_API, config_attr, context_attr);
local surface = egl_display:CreateWindowSurface(x_window.handle);
egl_display:MakeCurrent();
egl_display:PrintInfo();

-- the openGL part
local vsh = GLES2.LoadShader(GLES2.GL_VERTEX_SHADER, [[
   attribute vec4        position;
   varying mediump vec2  pos;
   uniform vec4          offset;

   void main()
   {
      gl_Position = position + offset;
      pos = position.xy;
   }]]);
local fsh = GLES2.LoadShader(GLES2.GL_FRAGMENT_SHADER, [[
   varying mediump vec2    pos;
   uniform mediump float   phase;

   void  main()
   {
      gl_FragColor = vec4(1., 0.9, 0.7, 1.0) *
//cos(30.*sqrt(pos.x*pos.x + 1.5*pos.y*pos.y - 1.8*pos.x*pos.y*pos.y) + atan(pos.y,pos.x) - phase);
//cos(30.*sqrt(pos.x*pos.x + 1.5*pos.y*pos.y) + atan(pos.y,pos.x) - phase);
//cos(20.*(pos.x*pos.x + pos.y*pos.y) - phase);
cos(20.*sqrt(pos.x*pos.x + pos.y*pos.y) + atan(pos.y,pos.x) - phase);

   }]]);
local shaderProgram = GLES2.CreateProgram(vsh, fsh);
local position_loc = GLES2.Lib.glGetAttribLocation(shaderProgram, "position");
local phase_loc = GLES2.Lib.glGetUniformLocation(shaderProgram, "phase");
local offset_loc = GLES2.Lib.glGetUniformLocation(shaderProgram, "offset");
local vertexArray = ffi.new("GLfloat[15]",
    0.0,  0.5,  0.0,
   -0.5,  0.0,  0.0,
    0.0, -0.5,  0.0,
    0.5,  0.0,  0.0,
    0.0,  0.5,  0.0);

-- setup
local win2view = function(x, y, attr)
   -- T = {{a, b, c}, {d, e, f}, {0, 0, 1}}.x
   -- p1 = T /. x -> {0, 0, 1}; p2 = T /. x -> {w, 0, 1}; p3 = T /. x -> {0, h, 1}
   -- Solve[{p1[[1]] == -1, p1[[2]] == 1, p2[[1]] == 1, p2[[2]] == 1, p3[[1]] == -1, p3[[2]] == -1}, {a, b, c, d, e, f}]
   -- A = T /. %; Z = A /. x -> {mx, my, 1}
   return -1. + (2.*x)/attr.width, 1. - (2.*y)/attr.height;
end
local _, _, _, _, pointer_x, pointer_y = x_window:XQueryPointer();
local gwa, phase = x_window:XGetWindowAttributes(), 0.;
local offset_x, offset_y = win2view(pointer_x, pointer_y, gwa);
GLES2.Lib.glViewport(0, 0, gwa.width, gwa.height);
GLES2.Lib.glClearColor(0.08, 0.06, 0.07, 1.);

local quit = false;
while not quit do
   -- update state
   GLES2.Lib.glUniform1f(phase_loc, phase);
   GLES2.Lib.glUniform4f(offset_loc, offset_x, offset_y, 0., 0.);

   -- handle events
   phase = (phase + 0.1) % (2. * 3.141);
   while x_display:XPending() > 0 do
      local xev = x_display:XNextEvent();
      if xev.type == Xlib.KeyPress then
         quit = true
      elseif xev.type == Xlib.MotionNotify then
         offset_x, offset_y = win2view(xev.xmotion.x, xev.xmotion.y, gwa);
      end
   end

   -- render
   GLES2.Lib.glClear(GLES2.GL_COLOR_BUFFER_BIT);
   GLES2.Lib.glVertexAttribPointer(position_loc, 3, GLES2.GL_FLOAT, false, 0, vertexArray);
   GLES2.Lib.glEnableVertexAttribArray(position_loc);
   GLES2.Lib.glDrawArrays(GLES2.GL_TRIANGLE_STRIP, 0, 5);

   -- swap buffers
   egl_display:SwapBuffers();
end

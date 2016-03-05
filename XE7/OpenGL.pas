unit OpenGL;

interface

uses
  DebugTools,
  Windows, Classes, SysUtils;

const
  GL_TRUE                             = 1;
  GL_FALSE                            = 0;

  GL_DEPTH_BUFFER_BIT                 = $00000100;
  GL_UNSIGNED_BYTE                    = $1401;
  GL_COLOR_BUFFER_BIT                 = $00004000;
  GL_VERSION                          = $1F02;
  GL_TEXTURE_2D                       = $0DE1;
  GL_RGB                              = $1907;
  GL_RGBA                             = $1908;
  GL_QUADS                            = $0007;

  GL_TEXTURE_ENV_MODE                 = $2200;
  GL_TEXTURE_ENV_COLOR                = $2201;
  GL_TEXTURE_ENV                      = $2300;
  GL_MODULATE                         = $2100;

  GL_TEXTURE_MAG_FILTER               = $2800;
  GL_TEXTURE_MIN_FILTER               = $2801;
  GL_TEXTURE_WRAP_S                   = $2802;
  GL_TEXTURE_WRAP_T                   = $2803;

  GL_REPEAT                           = $2901;

  GL_CLAMP_TO_BORDER                  = $812D;

  { TextureMagFilter }
  GL_NEAREST                          = $2600;
  GL_LINEAR                           = $2601;

  { TextureMinFilter }
  GL_NEAREST_MIPMAP_NEAREST           = $2700;
  GL_LINEAR_MIPMAP_NEAREST            = $2701;
  GL_NEAREST_MIPMAP_LINEAR            = $2702;
  GL_LINEAR_MIPMAP_LINEAR             = $2703;

  // GL_SGIS_generate_mipmap
  GL_GENERATE_MIPMAP_SGIS             = $8191;
  GL_GENERATE_MIPMAP_HINT_SGIS        = $8192;

type
  GLenum = Cardinal;
  GLboolean = BYTEBOOL;
  GLbitfield = Cardinal;
  GLbyte = Shortint;
  GLshort = SmallInt;
  GLint = Integer;
  GLsizei = Integer;
  GLubyte = Byte;
  GLushort = Word;
  GLuint = Cardinal;
  GLfloat = Single;
  GLclampf = Single;
  GLdouble = Double;
  GLclampd = Double;
  GLvoid = Pointer;
  GLint64 = Int64;
  GLuint64 = {$IFDEF DELPHI6_AND_DOWN} Int64 {$ELSE} UInt64 {$ENDIF};
  PGLuint = ^GLuint;

var
  isGL_Working : boolean = false;

function glGetString(name:GLenum):PAnsiChar;
function wglCreateContext(DC:HDC):HGLRC;
procedure glClear(mask:GLbitfield);
procedure glDrawPixels(width,height:GLsizei; format,pixeltype:GLenum; pixels:Pointer);
procedure glClearColor(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf);
procedure glEnable(cap: GLenum);
function gluBuild2DMipmaps(target: GLEnum; components, width, height: GLint; format, atype: GLEnum; const Data: Pointer): GLint;
procedure glBegin(mode: GLenum);
procedure glTexCoord2d(s: GLdouble; t: GLdouble);
procedure glVertex2d(x: GLdouble; y: GLdouble);
procedure glEnd;
procedure glFlush;
procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat);
procedure glGenTextures(n: GLsizei; textures:PGLuint);
procedure glBindTexture(target: GLenum; texture: GLuint);
procedure glTexEnvf(target: GLenum; pname: GLenum; param: GLfloat);
procedure glDisable(cap: GLenum);
procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint);

implementation

type
  TglGetString = function  (name: GLenum): PAnsiChar; stdcall;
  TwglCreateContext= function (DC: HDC): HGLRC; stdcall;
  TglClear = procedure (mask: GLbitfield); stdcall;
  TglDrawPixels = procedure (width, height: GLsizei; format, pixeltype: GLenum; pixels: Pointer); stdcall;
  TglClearColor = procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); stdcall;
  TglEnable = procedure(cap: GLenum); stdcall;
  TgluBuild2DMipmaps = function(target: GLEnum; components, width, height: GLint; format, atype: GLEnum; const Data: Pointer): GLint; stdcall;
  TglBegin = procedure(mode: GLenum); stdcall;
  TglTexCoord2d = procedure(s: GLdouble; t: GLdouble);stdcall;
  TglVertex2d = procedure(x: GLdouble; y: GLdouble); stdcall;
  TglEnd = procedure(); stdcall;
  TglFlush = procedure(); stdcall;
  TglTexParameterf = procedure(target: GLenum; pname: GLenum; param: GLfloat); stdcall;
  TglGenTextures = procedure(n: GLsizei; textures: PGLuint); stdcall;
  TglBindTexture = procedure(target: GLenum; texture: GLuint); stdcall;
  TglTexEnvf = procedure(target: GLenum; pname: GLenum; param: GLfloat); stdcall;
  TglDisable = procedure(cap: GLenum); stdcall;
  TglTexParameteri = procedure(target: GLenum; pname: GLenum; param: GLint); stdcall;

var
  dllHandle : Cardinal;
  GLU_LibHandle: Cardinal;

  glGetStringRef : TglGetString = nil;
  wglCreateContextRef : TwglCreateContext = nil;
  glClearRef : TglClear = nil;
  glDrawPixelsRef : TglDrawPixels = nil;
  glClearColorRef : TglClearColor = nil;
  glEnableRef : TglEnable = nil;
  gluBuild2DMipmapsRef : TgluBuild2DMipmaps = nil;
  glBeginRef : TglBegin = nil;
  glTexCoord2dRef : TglTexCoord2d = nil;
  glVertex2dRef : TglVertex2d = nil;
  glEndRef : TglEnd = nil;
  glFlushRef : TglFlush = nil;
  glTexParameterfReF : TglTexParameterf = nil;
  glGenTexturesRef : TglGenTextures = nil;
  glBindTextureRef : TglBindTexture = nil;
  glTexEnvfRef : TglTexEnvf = nil;
  glDisableRef : TglDisable = nil;
  glTexParameteriRef : TglTexParameteri = nil;

function glGetString(name:GLenum):PAnsiChar;
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glGetStringRef) then Result := glGetStringRef(name)
    else Result := nil;
  finally
    Set8087CW(Old8087CW);
  end;
end;

function wglCreateContext(DC:HDC):HGLRC;
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(wglCreateContextRef) then Result := wglCreateContextRef(DC)
    else Result := 0;
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glClear(mask:GLbitfield);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glClearRef) then glClearRef(mask);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glDrawPixels(width,height:GLsizei; format,pixeltype:GLenum; pixels:Pointer);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glDrawPixelsRef) then glDrawPixelsRef(width, height, format, pixeltype, pixels);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glClearColor(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glClearColorRef) then glClearColorRef(red, green, blue, alpha);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glEnable(cap: GLenum);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glEnableRef) then glEnableRef(cap);
  finally
    Set8087CW(Old8087CW);
  end;
end;

function gluBuild2DMipmaps(target: GLEnum; components, width, height: GLint; format, atype: GLEnum; const Data: Pointer): GLint;
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(gluBuild2DMipmapsRef) then gluBuild2DMipmapsRef(target, components, width, height, format, atype, Data);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glBegin(mode: GLenum);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glBeginRef) then glBeginRef(mode);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glTexCoord2d(s: GLdouble; t: GLdouble);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glTexCoord2dRef) then glTexCoord2dRef(s, t);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glVertex2d(x: GLdouble; y: GLdouble);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glVertex2dRef) then glVertex2dRef(x, y);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glEnd;
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glEndRef) then glEndRef;
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glFlush;
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glFlushRef) then glFlushRef;
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glTexParameterfRef) then glTexParameterfRef(target, pname, param);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glGenTextures(n: GLsizei; textures:PGLuint);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glGenTexturesRef) then glGenTexturesRef(n, textures);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glBindTexture(target: GLenum; texture: GLuint);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glBindTextureRef) then glBindTextureRef(target, texture);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glTexEnvf(target: GLenum; pname: GLenum; param: GLfloat);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glTexEnvfRef) then glTexEnvfRef(target, pname, param);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glDisable(cap: GLenum);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glDisableRef) then glDisableRef(cap);
  finally
    Set8087CW(Old8087CW);
  end;
end;

procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint);
var
  Old8087CW : word;
begin
  Old8087CW := Default8087CW;
  Set8087CW($133F);
  try
    if Assigned(glTexParameteriRef) then glTexParameteriRef(target, pname, param);
  finally
    Set8087CW(Old8087CW);
  end;
end;

initialization
  dllHandle := LoadLibrary( 'opengl32.dll' );
  if dllHandle = 0 then Exit;

  GLU_LibHandle := LoadLibrary('GLU32.dll');
  if GLU_LibHandle = 0 then Exit;

  @glGetStringRef := GetProcAddress( dllHandle, 'glGetString' );
  if not Assigned(glGetStringRef) then Trace('glGetStringRef is not ready.');

  @wglCreateContextRef := GetProcAddress( dllHandle, 'wglCreateContext' );
  if not Assigned(wglCreateContextRef) then Trace('wglCreateContextRef is not ready.');

  @glClearRef := GetProcAddress( dllHandle, 'glClear' );
  if not Assigned(glClearRef) then Trace('glClearRef is not ready.');

  @glDrawPixelsRef := GetProcAddress( dllHandle, 'glDrawPixels' );
  if not Assigned(glDrawPixelsRef) then Trace('glDrawPixelsRef is not ready.');

  @glClearColorRef := GetProcAddress( dllHandle, 'glClearColor' );
  if not Assigned(glClearColorRef) then Trace('glClearColorRef is not ready.');

  @glEnableRef := GetProcAddress( dllHandle, 'glEnable' );
  if not Assigned(glEnableRef) then Trace('glEnableRef is not ready.');

  @glBeginRef := GetProcAddress( dllHandle, 'glBegin' );
  if not Assigned(glBeginRef) then Trace('glBeginRef is not ready.');

  @glTexCoord2dRef := GetProcAddress( dllHandle, 'glTexCoord2d' );
  if not Assigned(glTexCoord2dRef) then Trace('glTexCoord2dRef is not ready.');

  @glVertex2dRef := GetProcAddress( dllHandle, 'glVertex2d' );
  if not Assigned(glVertex2dRef) then Trace('glVertex2dRef is not ready.');

  @glEndRef := GetProcAddress( dllHandle, 'glEnd' );
  if not Assigned(glEndRef) then Trace('glEndRef is not ready.');

  @glFlushRef := GetProcAddress( dllHandle, 'glFlush' );
  if not Assigned(glFlushRef) then Trace('glFlushRef is not ready.');

  @gluBuild2DMipmapsRef := GetProcAddress( GLU_LibHandle, 'gluBuild2DMipmaps' );
  if not Assigned(gluBuild2DMipmapsRef) then Trace('gluBuild2DMipmapsRef is not ready.');

  @glTexParameterfReF := GetProcAddress( dllHandle, 'glTexParameterf' );
  if not Assigned(glTexParameterfReF) then Trace('glTexParameterfReF is not ready.');

  @glGenTexturesRef := GetProcAddress( dllHandle, 'glGenTextures' );
  if not Assigned(glGenTexturesRef) then Trace('glGenTexturesRef is not ready.');

  @glBindTextureRef := GetProcAddress( dllHandle, 'glBindTexture' );
  if not Assigned(glBindTextureRef) then Trace('glBindTextureRef is not ready.');

  @glTexEnvfRef := GetProcAddress( dllHandle, 'glTexEnvf' );
  if not Assigned(glTexEnvfRef) then Trace('glTexEnvfRef is not ready.');

  @glDisableRef := GetProcAddress( dllHandle, 'glDisable' );
  if not Assigned(glDisableRef) then Trace('glDisableRef is not ready.');

  @glTexParameteriRef := GetProcAddress( dllHandle, 'glTexParameteri' );
  if not Assigned(glTexParameteriRef) then Trace('glTexParameteriRef is not ready.');

  isGL_Working :=
    Assigned(glGetStringRef) and
    Assigned(wglCreateContextRef) and
    Assigned(glClearRef) and
    Assigned(glDrawPixelsRef) and
    Assigned(glClearColorRef) and
    Assigned(glEnableRef) and
    Assigned(gluBuild2DMipmapsRef) and
    Assigned(glBeginRef) and
    Assigned(glTexCoord2dRef) and
    Assigned(glVertex2dRef) and
    Assigned(glEndRef) and
    Assigned(glTexParameterfReF) and
    Assigned(glGenTexturesRef) and
    Assigned(glBindTextureRef) and
    Assigned(glTexEnvfRef) and
    Assigned(glDisableRef) and
    Assigned(glTexParameteriRef) and
    Assigned(glFlushRef);
  if not isGL_Working then Trace( 'OpenGL is not ready to run.');
end.

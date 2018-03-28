{ First things first, check the platform target and be sure that we're hitting
  a Windows platform. }
{$IFNDEF MSWINDOWS}
  {$IFNDEF WIN32}
    {$IFNDEF WIN64}

      {$IFDEF LINUX}
      { This is actually a to-do thing for me, I'd like to implement Crt in Kylix.
        I imagine it will be heavily using ncurses, but who knows. =) }
        {$MESSAGE FATAL 'Delphi Crt unit is not supported on Linux/Kylix.'}
      {$ELSE}
      { This unit is only supported by Win32 and (I'm assuming) Win64 platforms.
        Borland, at the time of this files creation, hasn't committed to releasing
        a 64-bit Delphi compiler for either IA-64 or x86-64. }
      {$MESSAGE FATAL 'The Delphi Crt unit is only supported on Microsoft Windows platforms.'}
      !! DOES NOT WORK WITH NON-WINDOWS PLATFORMS !!
      {$ENDIF}  // LINUX
    {$ENDIF}  // WIN64
  {$ENDIF}  // WIN32
{$ENDIF}  // MSWINDOWS

{ Now do version detection. }

{$IFDEF CONDITIONALEXPRESSIONS}

  {$IF CompilerVersion >= 14}
    {$IFNDEF VER140}
      {$DEFINE VER140}
    {$ENDIF}  // IFNDEF VER140

    {$DEFINE VER130}
    {$DEFINE VER120}
    {$DEFINE VER100}
    {$DEFINE VER90}
    {$DEFINE VER80}

  {$IFEND}  // IF CompilerVersion >= 14

  {$IF CompilerVersion >= 15}
    {$IFNDEF VER150}
      {$DEFINE VER150}
    {$ENDIF}  // IFNDEF VER150
  {$IFEND}  // IF CompilerVersion >= 15

{$ELSE}   // IFDEF CONDITIONALEXPRESSIONS

  {$IFDEF VER80}      // Delphi 1
    // This unit only works with Delphi 2 and above
    !! DOES NOT WORK WITH DELPHI 1 OR BELOW !!
  {$ENDIF}  // IFDEF VER80

  {$IFDEF VER90}      // Delphi 2
    {$DEFINE VER80}
  {$ENDIF}  // IFDEF VER90

  {$IFDEF VER100}     // Delphi 3
    {$DEFINE VER90}
    {$DEFINE VER80}
  {$ENDIF}  // IFDEF VER100

  {$IFDEF VER120}     // Delphi 4
    {$DEFINE VER100}
    {$DEFINE VER90}
    {$DEFINE VER80}
  {$ENDIF}  // IFDEF VER120

  {$IFDEF VER130}     // Delphi 5
    {$DEFINE VER120}
    {$DEFINE VER100}
    {$DEFINE VER90}
    {$DEFINE VER80}
  {$ENDIF}  // IFDEF VER130

  {$IFDEF VER140}     // Delphi 6
    {$DEFINE VER130}
    {$DEFINE VER120}
    {$DEFINE VER100}
    {$DEFINE VER90}
    {$DEFINE VER80}
  {$ENDIF}  // IFDEF VER140

{$ENDIF}  // IFDEF CONDITIONALEXPRESSIONS


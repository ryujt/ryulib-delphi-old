unit FrameBase;

interface

uses
  Classes, SysUtils, Forms;

type
  IFrameBase = interface
    ['{4F736E5A-0A01-4D20-A76B-9DA0F3A78D0B}']

    procedure BeforeShow;
    procedure AfterShow;

    procedure BeforeClose;
  end;

implementation

end.

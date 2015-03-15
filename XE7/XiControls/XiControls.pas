{================================================================

    XiControls 0.01
    Written by Eugene Genev
    www.deadlogic.co.nr

    ---------------------------------------------------------

    Install this file (XiControls.pas) and all components
    should appear under XiControls tab of the components
    palette.

    Thank you for downloading XiControls.

=================================================================}

unit XiControls;

interface

uses
  Classes, XiButton, XiProgressBar, XiPanel, XiTrackBar;

implementation

{$R XiControls.res}

procedure Register;
begin
  RegisterComponents('XiControls', [TXiPanel, TXiTrackBar, TXiProgressBar, TXiButton]);
end;

end.

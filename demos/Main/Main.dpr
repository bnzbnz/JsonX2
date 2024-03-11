program Main;

uses
  System.StartUpCopy,
  FMX.Forms,
  W3DJsonX2.Obj in '..\..\W3DJsonX2.Obj.pas',
  W3DJsonX2 in '..\..\W3DJsonX2.pas',
  W3DJsonX2.RTTI in '..\..\W3DJsonX2.RTTI.pas',
  W3DJsonX2.Types in '..\..\W3DJsonX2.Types.pas',
  W3DJsonX2.Utils in '..\..\W3DJsonX2.Utils.pas',
  W3DCloneable in '..\..\W3DCloneable.pas',
  uMain in 'uMain.pas' {Form2},
  W3DJsonX2.Conv in '..\..\W3DJsonX2.Conv.pas',
  W3DJsonX2.Patch in '..\..\W3DJsonX2.Patch.pas',
  W3DJsonX2.Pool in '..\..\W3DJsonX2.Pool.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

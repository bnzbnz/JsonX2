program Large;

uses
  System.StartUpCopy,
  FMX.Forms,
  uLarge in 'uLarge.pas' {Form2},
  JsonX2.Conv in '..\..\JsonX2.Conv.pas',
  JsonX2.Obj in '..\..\JsonX2.Obj.pas',
  JsonX2 in '..\..\JsonX2.pas',
  JsonX2.Patch in '..\..\JsonX2.Patch.pas',
  JsonX2.RTTI in '..\..\JsonX2.RTTI.pas',
  JsonX2.Sync in '..\..\JsonX2.Sync.pas',
  JsonX2.Types in '..\..\JsonX2.Types.pas',
  JsonX2.Utils in '..\..\JsonX2.Utils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

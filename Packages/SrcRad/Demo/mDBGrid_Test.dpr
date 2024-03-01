program mDBGrid_Test;

uses
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  API.GridUtils in 'API\API.GridUtils.pas',
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

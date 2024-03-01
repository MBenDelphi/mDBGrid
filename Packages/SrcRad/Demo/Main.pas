unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Stan.StorageBin, API.DBGrid;

type
  TForm1 = class(TForm)
    mT_Test: TFDMemTable;
    ds_Test: TDataSource;
    mT_TestID: TAutoIncField;
    mT_TestUsername: TStringField;
    mT_TestPassword: TStringField;
    mT_TestTel: TStringField;
    mT_TestPicture: TBlobField;
    mT_TestEmail_PassRecovery: TStringField;
    mDBGrid_1: TmDBGrid_;
    procedure FormResize(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  API.GridUtils;

{$R *.dfm}

procedure TForm1.FormResize(Sender: TObject);
begin
  ScaleGrid(mDBGrid_1);
end;

end.

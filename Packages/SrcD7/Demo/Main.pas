unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, Grids, DBGrids, API.DBGrid, ExtCtrls, StdCtrls,
  DBCtrls, Buttons;

type
  TForm1 = class(TForm)
    mDBGrid_1: TmDBGrid_;
    Pnl_StatusBar: TPanel;
    Btn_Connect: TButton;
    DBNav_Test: TDBNavigator;
    ds_Test: TDataSource;
    Qry_User: TADOQuery;
    ADOCon_1: TADOConnection;
    procedure Btn_ConnectClick(Sender: TObject);
  private
    { Private declarations }
    function Connect_Access(const aMyDB_FullPath: string): boolean;
  public
    { Public declarations }
  end;

const
  MyDB_Name = 'DB.mdb';

var
  Form1: TForm1;

implementation

{$R *.dfm}

function Get_DB_FullPath: string; begin
  Result := ExtractFilePath(ParamStr(0))+'Data\'+ MyDB_Name;
end;

{ TForm1 }

function TForm1.Connect_Access(const aMyDB_FullPath: string): boolean;
var
  IsConnected: Boolean;
begin
  IsConnected := True;
  try
    ADOCon_1.LoginPrompt := False; // Disable login prompt

    ADOCon_1.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;' +    // provider for Access2000
                                       'Data Source='+aMyDB_FullPath+';'+       // databasefile
                                       'Mode=ReadWrite|Share Deny None;'+       // set to ReadWrite
                                       'Persist Security Info=False';

  finally

    try
      ADOCon_1.Open;
    except
      on E: Exception do begin
        IsConnected := False;
        Pnl_StatusBar.Caption := (E.Message);
      end;
    end;

  end;

  Result := IsConnected;
end;

procedure TForm1.Btn_ConnectClick(Sender: TObject);
begin
  case Btn_Connect.Tag of
    0:begin
       if Connect_Access(Get_DB_FullPath) then Qry_User.Open;
       Btn_Connect.Tag := 1;
       Btn_Connect.Caption := 'Disconnect !!';
    end;
    1:begin
       Qry_User.Close; ADOCon_1.Close;
       Btn_Connect.Tag := 0;
       Btn_Connect.Caption := 'Connect..';
    end;
  else
  end;
  

end;

end.

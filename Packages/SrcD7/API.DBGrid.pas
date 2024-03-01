unit API.DBGrid;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
//
  DB,
//
  Controls,
  Grids,
  DBGrids,
  StdCtrls,
  ExtCtrls,
  Forms, Graphics;

type
  TmDBGrid_ = class(TCustomDBGrid)
  private
    fSelectedColumn: Integer;
    fEdit_Search: TEdit;
    fLastIsFound: Boolean;
    fEditSearcFound_Color: TColor;
    fEditSearcNotFound_Color: TColor;
    fEditSearcStartEmpty_Color: TColor;
    fParentForm: TCustomForm;
    fParentFormCaption: string;
    procedure ApplyFilter(aColumnIndex: Cardinal; const aFilterText: string);
    procedure ShowEdit;
    procedure HideEdit;
    procedure UpdateEditPosition;
    procedure EditOnChange(Sender: TObject);
    procedure EditOnExit(Sender: TObject);
    function DataSet_OK: Boolean;
    procedure ParentFormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    procedure CreateWnd; override;
    procedure TitleClick(Column: TColumn); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EditSearcFound_Color: TColor read fEditSearcFound_Color
                                         write fEditSearcFound_Color stored True;

    property EditSearcNotFound_Color: TColor read fEditSearcNotFound_Color
                                         write fEditSearcNotFound_Color stored True;

    property EditSearcStartEmpty_Color: TColor read fEditSearcStartEmpty_Color
                                         write fEditSearcStartEmpty_Color stored True;

    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns stored False;
    property Constraints;
    property Ctl3D;
    property DataSource;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property ImeMode;
    property ImeName;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawDataCell;
    property OnDrawColumnCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
  end;

procedure Register;

implementation

uses Types;


procedure Register;
begin
  RegisterComponents('Data Controls', [TmDBGrid_]);
end;

{ TCustomFilteringGrid }

constructor TmDBGrid_.Create(AOwner: TComponent);
begin inherited;

 fEditSearcFound_Color := $0091EE91;
 fEditSearcNotFound_Color := $00BCB0FF;
 fEditSearcStartEmpty_Color := $00E7A0B7;
 fLastIsFound := False;

  fSelectedColumn := -1; // No column initially selected for filtering

  fEdit_Search := TEdit.Create(Self);
    fEdit_Search.Parent   := Self; // in order to OnChange event Edit triggered  correctlly..
    fEdit_Search.Visible  := False;
    fEdit_Search.AutoSize := False;
    fEdit_Search.Width    := 0;
    fEdit_Search.Height   := 0;
    fEdit_Search.Anchors  := [];
    fEdit_Search.Color := fEditSearcStartEmpty_Color;
//    fEdit.Clear; // do not call it here where the handle not created yet.. to avoid error control has no parent window!!
    fEdit_Search.OnExit   := EditOnExit;
    fEdit_Search.OnChange := EditOnChange;
end;

procedure TmDBGrid_.CreateWnd;
begin
  inherited;

  fParentForm         := TForm(Owner);
  fEdit_Search.Parent := fParentForm;

  TForm(fParentForm).OnMouseDown := ParentFormMouseDown;

end;

function TmDBGrid_.DataSet_OK: Boolean;
begin
  Result := False;
  if not Assigned(DataSource) then
    Exit else
    if not Assigned(DataSource.DataSet) then
      Exit else
      if not DataSource.DataSet.Active then
        Exit else Result := True;
end;

procedure TmDBGrid_.ShowEdit;
begin
  fParentFormCaption := fParentForm.Caption;

  UpdateEditPosition;

  if not fLastIsFound then fEdit_Search.Clear;
  
  fEdit_Search.Visible  := True;
  fEdit_Search.BringToFront;
  fEdit_Search.SetFocus;
end;

procedure TmDBGrid_.HideEdit;
var
  lDataSet: TDataSet;
begin
  lDataSet := DataSource.DataSet;
  if lDataSet.RecordCount = 0 then fEdit_Search.Clear;

  fEdit_Search.Visible       := False;
  TForm(fParentForm).Caption := fParentFormCaption;
end;

procedure TmDBGrid_.ParentFormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P.X := X; P.Y := Y;
  if not PtInRect(fEdit_Search.ClientRect, P) then  //(Sender as TForm).ScreenToClient(Mouse.CursorPos)
  HideEdit;
end;

procedure TmDBGrid_.UpdateEditPosition;
var
  R: TRect;
begin
  if (fSelectedColumn >= 0) and (fSelectedColumn < Columns.Count) then
  begin
    R := Self.CellRect(fSelectedColumn +1, 0);
    begin
      fEdit_Search.Height := Canvas.TextHeight('A') + 4;
      fEdit_Search.Top    := r.top + Self.Top +2;
      fEdit_Search.Left   := R.Left + Self.Left +2;
      fEdit_Search.Width  := r.Right - r.Left
    end;
  end;
end;

procedure TmDBGrid_.EditOnChange(Sender: TObject);
var
  lDataSet: TDataSet;
  fFilterText: string;
begin

  if not DataSet_OK then Exit;

  lDataSet := DataSource.DataSet;

  fFilterText := fEdit_Search.Text;

  if (fFilterText <> '') then  begin
    ApplyFilter(fSelectedColumn, fFilterText);
    if lDataSet.RecordCount > 0 then begin
      fEdit_Search.Color := fEditSearcFound_Color;
      fLastIsFound       := True;
    end else begin
      fEdit_Search.Color := fEditSearcNotFound_Color;
      fLastIsFound       := False;
    end;
  end else begin
    lDataSet.Filtered  := False;
    lDataSet.Filter    := '';
    fEdit_Search.Color := fEditSearcStartEmpty_Color;
  end;

end;

procedure TmDBGrid_.EditOnExit(Sender: TObject);
begin
  HideEdit;
end;

procedure TmDBGrid_.ApplyFilter(aColumnIndex: Cardinal; const aFilterText: string);
var
  lDataSet: TDataSet;
begin

  if DataSet_OK then begin
    lDataSet := DataSource.DataSet;
    lDataSet.DisableControls;
    try
//      lDataSet.FilterOptions := [foCaseInsensitive];
      lDataSet.Filtered := False;
      lDataSet.Filter   := '';
      lDataSet.Filter   := Columns[aColumnIndex].FieldName + ' LIKE ' +
                                    QuotedStr('%' + aFilterText + '%');
      lDataSet.Filtered := True;
    finally
      lDataSet.EnableControls;
    end;
  end;
end;

procedure TmDBGrid_.TitleClick(Column: TColumn);
var
  lDataSet: TDataSet;
begin inherited;

  if DataSet_OK then begin
    lDataSet := DataSource.DataSet;

    if (lDataSet.RecordCount > 0) then begin
      fSelectedColumn := Column.Index;
      fEdit_Search.Color     := fEditSearcStartEmpty_Color;
//      fEdit_Search.Alignment := Column.Title.Alignment;
      ShowEdit;
    end;
    TForm(fParentForm).Caption := 'Filtering by ['+ Columns[fSelectedColumn].FieldName +'] Column';
  end;
end;

procedure TmDBGrid_.Resize;
begin
  inherited;

  UpdateEditPosition;
end;

end.


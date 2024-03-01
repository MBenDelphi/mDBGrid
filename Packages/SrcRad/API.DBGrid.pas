unit API.DBGrid;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Data.DB,
//
  Vcl.Controls,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics;

type
  TControl = class(Vcl.Controls.TControl); // to access protected members

  TmDBGrid_ = class(TCustomDBGrid)
  private
    fSelectedColumn: Integer;
    fEdit_Search: TEdit;
    fLast_IsFound: Boolean;
    fLast_SelectedColumn: Integer;
    fEditSearch_Found_Color: TColor;
    fEditSearch_NotFound_Color: TColor;
    fEditSearch_StartEmpty_Color: TColor;
    fParentForm: TCustomForm;
    fParentFormCaption: string;
//    fAutoAdjustColumns: Boolean;
    procedure ApplyFilter(aColumnIndex: Cardinal; const aFilterText: string);
    procedure ShowEdit;
    procedure HideEdit;
    procedure UpdateEditPosition;
    procedure EditOnChange(Sender: TObject);
    procedure EditOnExit(Sender: TObject);
    function DataSet_OK: Boolean;
    procedure ParentFormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  strict private
    class constructor Create;
    class destructor Destroy;
  protected
    procedure CreateWnd; override;
    procedure TitleClick(Column: TColumn); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Canvas;
    property SelectedRows;
  published
    property EditSearcFound_Color: TColor read fEditSearch_Found_Color
                                         write fEditSearch_Found_Color stored True;

    property EditSearcNotFound_Color: TColor read fEditSearch_NotFound_Color
                                         write fEditSearch_NotFound_Color stored True;

    property EditSearcStartEmpty_Color: TColor read fEditSearch_StartEmpty_Color
                                         write fEditSearch_StartEmpty_Color stored True;

//    property AutoAdjustColumns: Boolean read fAutoAdjustColumns
//                                        write fAutoAdjustColumns stored True;


    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;[Stored(False)]
    property Columns stored False; //StoreColumns;
    property Constraints;
    property Ctl3D;
    property DataSource;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawingStyle;
    property Enabled;
    property FixedColor;
    property GradientEndColor;
    property GradientStartColor;
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
    property Touch;
    property Visible;
    property StyleElements;
    property StyleName;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawDataCell;  { obsolete }
    property OnDrawColumnCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
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

uses
  Vcl.Themes;

procedure Register;
begin
  RegisterComponents('Data Controls', [TmDBGrid_]);
end;

{ TCustomFilteringGrid }

constructor TmDBGrid_.Create(AOwner: TComponent);
begin inherited;

  fEditSearch_Found_Color      := $0091EE91;
  fEditSearch_NotFound_Color   := $00BCB0FF;
  fEditSearch_StartEmpty_Color := $00E7A0B7;
 
  fLast_IsFound        := False; // Logically No Search is Found Before..
  fLast_SelectedColumn := -1;    // Logically No column is Selected Before..
 
  fSelectedColumn      := -1; // No column initially selected for filtering

  fEdit_Search := TEdit.Create(Self);
    fEdit_Search.Parent   := Self; 
    fEdit_Search.Visible  := False;
    fEdit_Search.AutoSize := False;
    fEdit_Search.Width    := 0;
    fEdit_Search.Height   := 0;
    fEdit_Search.Anchors  := [];
    fEdit_Search.Color := fEditSearch_StartEmpty_Color;
//    fEdit.Clear; // do not call it here where the handle not created yet.. to avoid error control has no parent window!!
    fEdit_Search.OnExit   := EditOnExit;
    fEdit_Search.OnChange := EditOnChange;

end;

procedure TmDBGrid_.CreateWnd;
begin
  inherited;

  fParentForm         := GetParentForm(Self); //TForm(Owner);
  fEdit_Search.Parent := fParentForm;  // in order to OnChange event Edit triggered  correctlly..

  TForm(fParentForm).OnMouseDown := ParentFormMouseDown;

end;

function TmDBGrid_.DataSet_OK: Boolean;
begin
  if not Assigned(DataSource) then
    Exit(False) else
    if not Assigned(DataSource.DataSet) then
      Exit(False) else
      if not DataSource.DataSet.Active then
        Exit(False) else Result := True;
end;

destructor TmDBGrid_.Destroy;
begin

  inherited;
end;

procedure TmDBGrid_.ShowEdit;
begin
  fParentFormCaption := fParentForm.Caption;

  UpdateEditPosition;

  if not fLast_IsFound then fEdit_Search.Clear;
  if fLast_IsFound then
    if (fLast_SelectedColumn <> fSelectedColumn) then fEdit_Search.Clear;
  
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
begin
  if not PtInRect(fEdit_Search.ClientRect, TPoint.Create(X, Y)) then  //(Sender as TForm).ScreenToClient(Mouse.CursorPos)
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
      fEdit_Search.Width  := R.Width;
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
      fEdit_Search.Color   := fEditSearch_Found_Color;
      fLast_IsFound        := True;
      fLast_SelectedColumn := fSelectedColumn;
    end else begin
      fEdit_Search.Color := fEditSearch_NotFound_Color;
      fLast_IsFound      := False;
    end;
  end else begin
    lDataSet.Filtered  := False;
    lDataSet.Filter    := '';
    fEdit_Search.Color := fEditSearch_StartEmpty_Color;
  end;

end;

procedure TmDBGrid_.EditOnExit(Sender: TObject);
begin
  HideEdit;
end;

//procedure TmDBGrid_.GridCellClick(Column: TColumn);
//begin
//  HideEdit;
//end;
//
//procedure TmDBGrid_.GridEditButtonClick(Sender: TObject);
//begin
//  HideEdit;
//end;
//
//procedure TmDBGrid_.GridMouseDown(Sender: TObject;
//  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//begin
//  HideEdit;
//end;

procedure TmDBGrid_.ApplyFilter(aColumnIndex: Cardinal; const aFilterText: string);
var
  lDataSet: TDataSet;
begin

  if DataSet_OK then begin
    lDataSet := DataSource.DataSet;
    lDataSet.DisableControls;
    try
      lDataSet.FilterOptions := [foCaseInsensitive];
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
      fSelectedColumn        := Column.Index;
      fEdit_Search.Color     := fEditSearch_StartEmpty_Color;
      fEdit_Search.Alignment := Column.Title.Alignment;
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

//procedure TmDBGrid.ParentFormResize(Sender: TObject);
//begin
//  if fAutoAdjustColumns then ScaleGrid(TDBGrid(Self));
//
//end;

class constructor TmDBGrid_.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TmDBGrid_, TScrollingStyleHook);
end;

class destructor TmDBGrid_.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TmDBGrid_, TScrollingStyleHook);
end;

end.

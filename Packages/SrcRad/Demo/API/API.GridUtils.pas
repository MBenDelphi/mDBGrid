unit API.GridUtils;

interface

uses
  SysUtils,
  Classes,
  DBGrids,
  DB, API.DBGrid;

type
  TArrayInt =  Array  of  Integer;

    function DataSet_OK(aDBGrid : TDBGrid ): Boolean;
    procedure ScaleGrid (aDBGrid : TmDBGrid_ );

implementation

function DataSet_OK(aDBGrid : TDBGrid ): Boolean;
begin
  if not Assigned(aDBGrid.DataSource) then
    Exit(False) else
    if not Assigned(aDBGrid.DataSource.DataSet) then
      Exit(False) else
      if not aDBGrid.DataSource.DataSet.Active then
        Exit(False) else Result := True;
end;

procedure ScaleGrid (aDBGrid : TmDBGrid_ ) ;
  procedure AdjustColumns ( Swidth , TSize :  Integer ; Asize : TArrayInt ) ;
  var
    idx :  Integer ;
  begin
    if TSize =  0  then
    begin
      TSize := aDBGrid.Columns.count;
      for idx :=  0  to aDBGrid.Columns.count  -1  do
        aDBGrid.Columns[ idx ].Width  :=  (aDBGrid.Width  - aDBGrid.Canvas.TextWidth('AAAAAA')) div TSize;
    end else
      for idx := 0  to aDBGrid.Columns.count - 1  do
        aDBGrid.Columns[ idx ].Width  := aDBGrid.Columns[ idx ].Width  +
          ( Swidth * Asize [ idx ]  div TSize );
  end ;

var
  idx , Twidth , TSize , Swidth:  Integer ;
  AWidth : TArrayInt ;
  Asize : TArrayInt ;
  ColumnName :  String ;
begin
//  if not DataSet_OK(aDBGrid) then Exit;

  SetLength ( AWidth , aDBGrid . Columns.count ) ;
  SetLength ( Asize , aDBGrid . Columns.count ) ;
  Twidth :=  0 ;
  TSize :=  0 ;
  for idx :=  0  to aDBGrid . Columns.count  - 1  do
  begin
    ColumnName := aDBGrid . Columns[ idx ].Title.Caption ;
    aDBGrid.Columns[ idx ].Width := aDBGrid.Canvas.TextWidth
      ( aDBGrid.Columns[ idx ].Title.Caption  +  'A' ) ;
    AWidth [ idx ]  := aDBGrid . Columns [ idx ].Width ;
    Twidth := Twidth + AWidth [ idx ] ;

    if  Assigned ( aDBGrid . Columns [ idx ] . Field )  then
      Asize [ idx ]  := aDBGrid . Columns [ idx ] . Field . Size
    else
      Asize [ idx ]  :=  1 ;

    TSize := TSize + Asize [ idx ] ;
  end;
  if TDBGridOption . dgColLines  in aDBGrid . Options  then
    Twidth := Twidth + aDBGrid . Columns . count ;

  // add the indicated column width of the cursor
  if TDBGridOption . dgIndicator  in aDBGrid . Options  then
    Twidth := Twidth + IndicatorWidth ;

  Swidth := aDBGrid . ClientWidth  - Twidth ;
  AdjustColumns ( Swidth , TSize , Asize ) ;
end ;

end.

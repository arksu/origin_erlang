unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Generics.Collections;

const
  GRID_SIZE = 100;
  WIDTH_DATA = 1000;
  HEIGHT_DATA = 1000;

type
  TGridRow = array[0..GRID_SIZE-1] of byte;

  TGrid = class
  public
    data : array[0..9999] of byte;
    server_data : array[0..19999] of byte;
    num : Integer;

    constructor Create(num : Integer);
    procedure write(row: Integer; buf : TGridRow);
    procedure save();
    procedure process;
  end;

  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    grids : TDictionary<Integer,TGrid>;

    procedure save_grids();
  end;


var
  Form1: TForm1;


implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  f : TFileStream;
  x, y, grid_num : Integer;
  buf : TGridRow;
  grid : TGrid;
begin
  f := TFileStream.Create(edit1.Text, fmOpenRead);

  for y := 0 to (HEIGHT_DATA div GRID_SIZE) - 1 do
    for x := 0 to (WIDTH_DATA div GRID_SIZE) - 1 do
    begin
      grid_num := y * 50 + x;
      grid := TGrid.Create(grid_num);
      grids.Add(grid_num, grid);
    end;

  for y := 0 to (HEIGHT_DATA) - 1 do
    for x := 0 to (WIDTH_DATA div GRID_SIZE) - 1 do
    begin
      // вычисляем номер грида
      grid_num := (y div GRID_SIZE) * 50 + x;
      // читаем в буфер
      f.ReadBuffer(buf[0], GRID_SIZE);
      // берем грид
      grids.TryGetValue(grid_num, grid);
      // пихаем данные в грид
      grid.write(y mod GRID_SIZE, buf);
    end;
  f.Free;

  save_grids;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  grids := TDictionary<Integer,TGrid>.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  g : TGrid;
begin
  for g in grids.Values do
    g.Free;

  grids.Clear;
  grids.Free;
end;

procedure TForm1.save_grids;
var
  list : TDictionary<Integer,TGrid>.TValueCollection;
  g : tgrid;
begin
  list := grids.Values;

  for g in list do
  begin
    g.process;
    g.save;
  end;
end;

{ TGrid }

constructor TGrid.Create(num: Integer);
begin
  self.num := num;
end;

procedure TGrid.process;
var
  i : Integer;
  lv, b2 : byte;
begin
  for i := 0 to 9999 do
  begin
    lv := data[i];

    b2 := lv;
    if (lv < 15) then
    begin
      b2 := lv + 128;
      server_data[i] := 10;
    end;
    if (lv >= 15) and (lv < 16) then
      server_data[i] := 10;
    if (lv >= 16) then
      server_data[i] := 35;


    server_data[10000+i] := 0;
  end;

end;

procedure TGrid.save;
var
  f : TFileStream;
  fname : string;
  i : Integer;
begin
  fname := './out/grid_'+IntToStr(num)+'.map';
  if FileExists(fname) then
    DeleteFile(fname);

  f := TFileStream.Create(fname, fmCreate);
  f.WriteBuffer(server_data[0], 20000);
  f.Free;
end;

procedure TGrid.write(row: Integer; buf: TGridRow);
var
  i : Integer;
begin
  for i := 0 to GRID_SIZE-1 do
    data[row*GRID_SIZE+i] := buf[i];
end;

end.

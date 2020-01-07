unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pngimage, ExtCtrls;

const
  GRID_SIZE = 100;
  SUPERGRID_SIZE = 50;
  TILE_SIZE = 12;

type
  TMapGrid = record
    bin_data : array[0..100*100*2-1] of Byte;
    procedure save(num : Integer);
  end;

  TObj = packed record
    x : integer;
    y : Integer;
    atype : Byte;
  end;

  Tmain_form = class(TForm)
    Button1: TButton;
    Button2: TButton;
    od: TOpenDialog;
    Button3: TButton;
    Timer1: TTimer;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    map_data : array of TMapGrid;
    obj_data : array of TObj;
    obj_count : Integer;

    procedure Process();
    procedure log_print(msg : string);
    function get_tile(c : TColor) : Byte;
    function get_obj(c : TColor) : Byte;
    procedure add_obj(tx, ty : Integer; atype : Byte);
    procedure save_map();
    procedure save_obj();
  end;




var
  main_form: Tmain_form;
  tiles_fname, obj_fname : string;
  is_error : boolean = False;
  is_autostart : Boolean = False;

implementation

{$R *.dfm}

procedure Tmain_form.add_obj(tx, ty: Integer; atype: Byte);
begin
  Inc(obj_count);

  if obj_count > Length(obj_data) then
    SetLength(obj_data, Length(obj_data)+200);

  obj_data[obj_count-1].x := tx*TILE_SIZE;
  obj_data[obj_count-1].y := ty*TILE_SIZE;
  obj_data[obj_count-1].atype := atype;
end;

procedure Tmain_form.Button1Click(Sender: TObject);
begin
  if od.Execute then begin
    tiles_fname := od.FileName;
  end;

end;

procedure Tmain_form.Button2Click(Sender: TObject);
begin
  if od.Execute then begin
    obj_fname := od.FileName;
  end;

end;

procedure Tmain_form.Button3Click(Sender: TObject);
begin
  Process;
end;

procedure Tmain_form.FormShow(Sender: TObject);
begin
  if ParamStr(1) <> '' then
  begin
    tiles_fname := ParamStr(1);
    obj_fname := ParamStr(2);

    is_autostart := true;
    Timer1.Enabled := True;
  end;

end;

function Tmain_form.get_obj(c: TColor): Byte;
var
  s : string;
begin
  FmtStr(s, '%s%0.8x', [HexDisplayPrefix, Integer(c)]);
//  s := Graphics.ColorToString(c);
  s := s[8]+s[9]+s[6]+s[7]+s[4]+s[5];
  //log_print(s);

  // fir
  if s = 'FF0000' then Exit(1);
  // stone
  if s = '000000' then Exit(2);
  // oak
  if s = '32CD32' then Exit(3);
  // birch
  if s = '008000' then Exit(4);

  Result := 0;
  log_print('warning unknown obj: '+s);
  is_error := True;
end;

function Tmain_form.get_tile(c: TColor): Byte;
var
  s : string;
begin
  s := Graphics.ColorToString(c);
  s := s[8]+s[9]+s[6]+s[7]+s[4]+s[5];
  //log_print(s);

  // forest fir
  if s = '066101' then Exit(30);
  // grass
  if s = '5ACC51' then Exit(35);
  // low water
  if s = '988DFF' then Exit(2);
  // deep water
  if s = '1800FF' then Exit(1);

  Result := 0;
  log_print('warning unknown tile: '+s);
  is_error := True;
end;

procedure Tmain_form.log_print(msg: string);
begin
  memo1.Lines.Add(msg);
end;

procedure Tmain_form.Process;
var
  tiles, objs : TPngImage;

  procedure fill_map();
  var
    i, j, r, c : Integer;
    row : TByteArray;
    col : TColor;
  begin
    SetLength(map_data, SUPERGRID_SIZE*SUPERGRID_SIZE);
    for i := 0 to SUPERGRID_SIZE*SUPERGRID_SIZE - 1 do
      FillChar(map_data[i].bin_data, GRID_SIZE*GRID_SIZE*2, 0);

    for i := 0 to SUPERGRID_SIZE - 1 do begin
    log_print('current supergrid row: '+IntToStr(i)+'/'+IntToStr(SUPERGRID_SIZE));
    for j := 0 to SUPERGRID_SIZE - 1 do
    begin
      // построчно в пределах 1 грида
      for r := 0 to GRID_SIZE - 1 do
      for c := 0 to GRID_SIZE - 1 do
      begin
        col := tiles.Pixels[j*GRID_SIZE+c, i*GRID_SIZE+r];
        map_data[i*SUPERGRID_SIZE+j].bin_data[r*GRID_SIZE+c] := get_tile(col);
      end;
    end;
    end;
  end;

  procedure fill_obj;
  var
    i, j : Integer;
    col : TColor;
    a : byte;
  begin
    log_print('obj size='+IntToStr(SizeOf(tobj)));
    for i := 0 to GRID_SIZE*SUPERGRID_SIZE-1 do
    for j := 0 to GRID_SIZE*SUPERGRID_SIZE-1 do
    begin
      a := (objs.AlphaScanline[i])^[j];
      if (a > 100) then begin
        col := objs.Pixels[j, i];
        add_obj(j, i, get_obj(col));
      end;
    end;

  end;

begin
  SetLength(obj_data, 0);
  obj_count := 0;

  tiles := TPngImage.Create;
  log_print('load tiles...');
  tiles.LoadFromFile(tiles_fname);
  if (tiles.Width <> GRID_SIZE*SUPERGRID_SIZE) or (tiles.Height <> GRID_SIZE*SUPERGRID_SIZE) then begin
    MessageBox(Application.Handle, 'Размер пнг не корректен','Ебаца в тепловизор!', MB_ICONERROR);
    is_error := True;
  end;

  objs := TPngImage.Create;
  log_print('load objects...');
  objs.LoadFromFile(obj_fname);
  if (objs.Width <> GRID_SIZE*SUPERGRID_SIZE) or (objs.Height <> GRID_SIZE*SUPERGRID_SIZE) then begin
    MessageBox(Application.Handle, 'Размер пнг не корректен','Ебаца в тепловизор!', MB_ICONERROR);
    is_error := True;
  end;

  if (not is_error) then begin
     fill_map;
     save_map;

     fill_obj;
     save_obj;
  end;

  tiles.Free;
  objs.Free;

  if (not is_error) and is_autostart then
    Application.Terminate;

end;

procedure Tmain_form.save_map;
var
  i : Integer;
begin
  log_print('saving...');
  for i := 0 to SUPERGRID_SIZE*SUPERGRID_SIZE - 1 do
  begin
    map_data[i].save(i);
  end;
  log_print('saved!');
end;

procedure Tmain_form.save_obj;
var
  f : TFileStream;
  fname : string;
  i : Integer;
begin
  log_print('save objects...');
  fname := './out/objects.bin';
  if FileExists(fname) then
    DeleteFile(fname);

  f := TFileStream.Create(fname, fmCreate);
  for i := 0 to obj_count - 1 do
  begin
    f.WriteBuffer(obj_data[i], SizeOf(Tobj));
  end;
  f.Free;
end;

procedure Tmain_form.Timer1Timer(Sender: TObject);
begin
  Process;
end;

{ TMapGrid }

procedure TMapGrid.save(num : Integer);
var
  f : TFileStream;
  fname : string;
  i : Integer;
begin
  fname := './out/grid_'+IntToStr(num)+'.map';
  if FileExists(fname) then
    DeleteFile(fname);

  f := TFileStream.Create(fname, fmCreate);
  f.WriteBuffer(bin_data[0], 20000);
  f.Free;
end;

end.

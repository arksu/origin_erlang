program map_import;

uses
  Forms,
  main in 'main.pas' {main_form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tmain_form, main_form);
  Application.Run;
end.

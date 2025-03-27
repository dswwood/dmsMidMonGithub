unit dswIni;

{ --------------------------------------------------------------------------- }
                                interface
{ --------------------------------------------------------------------------- }

{ To use:
    AppIni.ReadString( ini_sec_, ini_key_, '12345' );
    AppIni.WriteString( ini_sec_, ini_key_, Value );
}

uses
  classes, sysutils, inifiles, forms;

const
  INI_SEC_MIDI = 'MIDI';
  INI_KEY_MIDIIN = 'Midi In Device';
  INI_KEY_MIDIOUT = 'Midi Out Device';
  INI_SYSEX_DELAY = 'Sysex Delay';

  INI_SEC_FORM_COORDS = 'Form Size/Position';


function AppIni : TIniFile;

procedure SaveFormCoords( aForm : TForm );
procedure RestoreFormCoords( aForm : TForm );


{ --------------------------------------------------------------------------- }
                               implementation
{ --------------------------------------------------------------------------- }

var
  g_IniFile : TIniFile;


function CreateAppIni : TInifile;
var
  fn: string;
  i: Integer;
begin

  { Find the .ini file:
    First, search the command line for '.ini' file name.
    If not there, use the program directory.
  }

  fn := '';
  for i := 1 to paramcount do begin
    if pos('.ini', lowercase(paramstr(i))) > 0 then begin
      fn := ExpandFileName(paramstr(i));
      break;
    end;
  end;

  // If not found on the command line. Use exe directory.
  if fn = '' then
    fn := ExpandFileName(ChangeFileExt( ExtractFileName(paramstr(0)), '.ini'));

  result := TiniFile.Create(fn);
end;

function AppIni : TIniFile;
begin
  if not Assigned( g_IniFile )
    then g_IniFile := CreateAppIni;
  Result := g_IniFile;
end;

procedure SaveFormCoords(aForm: TForm);
begin
  AppIni.WriteInteger(aForm.Name, 'left', aForm.Left);
  AppIni.WriteInteger(aForm.Name, 'top', aForm.Top);
  AppIni.WriteInteger(aForm.Name, 'height', aForm.Height);
  AppIni.WriteInteger(aForm.Name, 'width', aForm.Width);
end;

procedure RestoreFormCoords(aForm: TForm);
begin
  if AppIni.SectionExists(aForm.Name) then begin
    aForm.Left := AppIni.ReadInteger(aForm.Name, 'left', aForm.Left);
    aForm.Top := AppIni.ReadInteger(aForm.Name, 'top', aForm.Top);
    aForm.Height := AppIni.ReadInteger(aForm.Name, 'height', aForm.Height);
    aForm.Width := AppIni.ReadInteger(aForm.Name, 'width', aForm.Width);
    aform.MakeFullyVisible; // Account for turned off monitor, etc.
  end;
end;



initialization
  g_IniFile := nil;

finalization
  FreeAndNil(g_IniFile);

end.

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, Messages,
  JWAwindows, midi;

type

  { TMainForm }

  TMainForm = class(TForm)
    AsciiToHexStringButton: TButton;
    ConvertButton: TButton;
    DumpTDWordButton: TButton;
    DeleteMsgButton: TButton;
    Label4: TLabel;
    SysexDelayLabel: TLabel;
    TestRolandChecksumButton: TButton;
    MidiMsgMemo: TMemo;
    Panel1: TPanel;
    SaveMsgButton: TButton;
    SaveMsgComboBox: TComboBox;
    DebugMemo: TMemo;
    SaveMsgGroupBox: TGroupBox;
    HexStringToAsciiBitBtn: TBitBtn;
    Splitter2: TSplitter;
    TestMidiEdit: TLabeledEdit;
    MidiSendMemo: TMemo;
    MidiSendButton: TButton;
    SysExMemo: TMemo;
    MidiSendGroupBox: TGroupBox;
    MidiMsgGroupBox: TGroupBox;
    SysexGroupBox: TGroupBox;
    MidiInCheckBox: TCheckBox;
    MidiOutCheckbox: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MidiInListBox: TListBox;
    MidiOutListBox: TListBox;
    MidiMonPageControl: TPageControl;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    SettingsTabSheet: TTabSheet;
    MidiMonTabSheet: TTabSheet;
    CalcTabSheet: TTabSheet;
    SysexDelayTrackBar: TTrackBar;
    procedure AsciiToHexStringButtonClick(Sender: TObject);
    procedure ConvertButtonClick(Sender: TObject);
    procedure DeleteMsgButtonClick(Sender: TObject);
    procedure DumpTDWordButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HexStringToAsciiBitBtnClick(Sender: TObject);
    procedure MidiListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure MidiSendButtonClick(Sender: TObject);
    procedure MidiToggleCheckBoxChange(Sender: TObject);
    procedure TestRolandChecksumButtonClick(Sender: TObject);
    procedure SaveMsgButtonClick(Sender: TObject);
    procedure SaveMsgComboBoxSelect(Sender: TObject);
    procedure SysexDelayTrackBarChange(Sender: TObject);
    procedure SysexDelayTrackBarExit(Sender: TObject);
  private
    fSysExBufferStr : string;

    // Milliseconds to wait between sending messages, to give devices time to digest them
    MsToWaitBetweenSysex : dword;
    // time last message was sent to calc delay
    MsLastSysexSent : dword;
    MsLastSendClick : dword; // to avoid double clicking the send button

    function GetMidiInDevice(showerr : boolean = false) : integer;
    function GetMidiOutDevice(showerr : boolean = false) : integer;

    procedure EnableDisableControls;

    procedure OnMidiInDataCALLBACK( const aDeviceIndex: integer; const aStatus, aData1, aData2: byte);
    procedure OnSysExDataCALLBACK( const aDeviceIndex: integer; const aStream: TMemoryStream);

    procedure WmMidiDataArrived( var Msg: TMessage); message WM_MIDIDATA_ARRIVED;
    procedure WmSysexDataArrived( var Msg: TMessage); message WM_MIDISYSEX_ARRIVED;
    procedure WmMidiInError( var Msg: TMessage); message WM_MIM_ERROR;
    procedure WmLongError( var Msg: TMessage); message WM_MIM_LONGERROR;

  public
    procedure ReloadMsgCombo;
  end;

var
  MainForm: TMainForm;

implementation

uses
  lcltype, inifiles, dswIni, dmsMidiUtil, msgdb, mmsystem;

{$R *.lfm}

const
  COMMENT_CHAR = ';';
  DOUBLE_CLICK_TIME_MS = 500;

{ TMainForm }

{%region Form GUI Events}
procedure TMainForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
  dswIni.RestoreFormCoords(self);

  MsToWaitBetweenSysex := AppIni.ReadInt64(INI_SEC_MIDI, INI_SYSEX_DELAY, 0);
  SysexDelayTrackBar.Position := MsToWaitBetweenSysex;

  MsLastSysexSent := timegettime;
  MsLastSendClick := timegettime;

  MidiInput.OnMidiData:= @Self.OnMidiInDataCALLBACK;
  MidiInput.OnSysExData := @Self.OnSysExDataCALLBACK;

  // Load the listboxes
  for i := 0 to MidiInput.Devices.Count -1 do
    MidiInListbox.Items.Add(MidiInput.Devices[i]);

  for i := 0 to MidiOutput.Devices.Count -1 do
    MidiOutListbox.Items.Add(MidiOutput.Devices[i]);

  // load previous device selections from ini file and open them
  MidiInListBox.ItemIndex := MidiInListBox.Items.IndexOf(
    AppIni.ReadString(INI_SEC_MIDI, INI_KEY_MIDIIN, '')
  );
  MidiInCheckBox.Checked := MidiInListBox.ItemIndex >= 0;

  MidiOutListBox.ItemIndex := MidiOutListBox.Items.IndexOf(
    AppIni.ReadString(INI_SEC_MIDI, INI_KEY_MIDIOUT, '')
  );
  MidiOutCheckBox.Checked := MidiOutListBox.ItemIndex >= 0;

  if GetMidiInDevice + GetMidiOutDevice >= -1 then // if either are opened..
    MidiMonPageControl.ActivePageIndex := 0  // Monitor page
  else
    MidiMonPageControl.ActivePageIndex := 1; // Settings page

  // Load saved midi messages
  ReloadMsgCombo;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MidiInput.CloseAll;
  MidiOutput.CloseAll;
  dswIni.SaveFormCoords(self);
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  midi.midi_debug_strs := DebugMemo.Lines;
end;

//
// This is needed because the default selected item color clashes with
// the highlight color, making the item impossible to read. wtf?
//
procedure TMainForm.MidiListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  lb : TListbox;
begin
  lb := TListBox(Control);

  if odSelected in State then begin
    LB.Canvas.Brush.Color := clHighLight;
    LB.Canvas.Font.Color := clHighlightText;
  end else begin
    LB.Canvas.Brush.Color := clWindow;
    if lb.enabled then
      lb.Canvas.Font.Color := clDefault
    else
      lb.Canvas.Font.Color := clGray
  end;

  lb.Canvas.FillRect(ARect);
  LB.Canvas.TextRect(ARect, ARect.Left + 2, ARect.Top + 2, LB.Items[Index]);

  if (odFocused in State) and not (odNoFocusRect in State) then
    LB.Canvas.DrawFocusRect(ARect);
end;

{%endregion Form GUI Events}

{%region Calc Tab}
procedure TMainForm.HexStringToAsciiBitBtnClick(Sender: TObject);
begin
  try
    debugmemo.append(BytesToAscii( HexStringToBytes(TestMidiEdit.Text) ));
  except
    on e:exception do debugmemo.append(e.Message);
  end;
end;


procedure TMainForm.AsciiToHexStringButtonClick(Sender: TObject);
begin
  try
    debugmemo.append(AsciiStrToHexStr(TestMidiEdit.Text));
  except
    on e:exception do debugmemo.append(e.Message);
  end;
end;


procedure TMainForm.ConvertButtonClick(Sender: TObject);
begin
  try
    debugmemo.append( TestMidiEdit.Text + ':'^M^J + ConvertNumber(TestMidiEdit.Text) );
  except
    on E: Exception do
      debugmemo.append( E.Message );
  end;
end;


procedure TMainForm.DumpTDWordButtonClick(Sender: TObject);
var
  s, st : string;
  dw : TDWord;
  bts : TBytes;
begin
  s := trim(TestMidiEdit.text);
  if length(s) = 0 then
    exit;

  dw.DWord4 := 0;
  try
    // Try to convert the text to a tdword
    if s[length(s)] = 'd' then begin // decimal
      dw.DWord4 := StrToInt( copy(s, 1, length(s)-1 ));
    end else
    if s[length(s)] = 'b' then begin // binary
      dw.DWord4 := StrToInt( '%' + copy(s, 1, length(s)-1 ));
    end else begin // hex
      bts := HexStringToBytes(s);
      if length(bts) > 4 then begin
        showmessage('Input format should be "F0 F1 F2 F3" for hex, ' +
                     ^M^J + '"12345d" for decimal, ' + ^M^J + ' or 10101010b for binary');
        exit;
      end else begin
        st := StringReplace( AnsiUpperCase(s), ' ', '', [rfReplaceAll]);
        dw.DWord4 := StrToInt( '$' + st );
      end;
    end;

    debugmemo.append(^M^J'Dump "' + s + '"');
    DumpTDword(dw, debugmemo.lines);
  except
    on e:exception do debugmemo.append(e.Message);
  end;
end;


procedure TMainForm.TestRolandChecksumButtonClick(Sender: TObject);
begin
  try
    Debugmemo.Append( ParseRolandChecksum(TestMidiEdit.Text) );
  except
    on e:exception do debugmemo.append(e.Message);
  end;
end;


{%endregion Calc Tab}

procedure TMainForm.MidiSendButtonClick(Sender: TObject);
var
  s, s2 : ansistring;
  i : integer;
  mv : TShortMidiMsgRecord;
  DevIndex : integer;
begin
  // avoid doubleclicks
  if timegettime - MsLastSendClick < DOUBLE_CLICK_TIME_MS then
    exit;
  MsLastSendClick := timegettime;

  DevIndex := GetMidiOutDevice(true);
  if DevIndex < 0 then
    exit;

  for i := 0 to MidiSendMemo.Lines.Count-1 do begin
    s := trim(MidiSendMemo.Lines[i]);
    if (length(s) = 0) or (s[1] = COMMENT_CHAR) then
      continue;

    if pos('F0', s) = 1 then begin
      s2 := ParseRolandChecksum(s);
      debugmemo.append('sending sysex: "' + s2 + '"');

      // Delay sysex
      while int64(timegettime - MsLastSysexSent) < MsToWaitBetweenSysex do
        sleep(10);
      MsLastSysexSent := timegettime; // initialize to current time

      MidiOutput.SendSysEx(DevIndex, s2);
    end else begin
      //debugmemo.Append('sending midimsg = ' + s);
      mv := StrToShortMidiMsg(s);
      MidiOutput.Send( DevIndex, mv.aStatus, mv.aData1, mv.aData2 );
    end;
    Application.ProcessMessages;
  end; // for
end;


procedure TMainForm.MidiToggleCheckBoxChange(Sender: TObject);
var
  ix : integer;
  bopen : boolean;
  device : TMidiDevices;
  lb : TListbox;
  inikey : string;
begin
  bopen := TCheckBox(Sender).Checked;
  if Sender = MidiInCheckBox then begin
    device := MidiInput;
    lb := MidiInListBox;
    inikey := INI_KEY_MIDIIN;
  end else begin
    device := MidiOutput;
    lb := MidiOutListBox;
    inikey := INI_KEY_MIDIOUT;
  end;

  if bopen then begin
    ix := lb.ItemIndex;
    if ix < 0 then begin
      ShowMessage('No device selected');
      TCheckBox(Sender).Checked := false;
      AppIni.DeleteKey(INI_SEC_MIDI, inikey);
    end else begin
      device.Open(ix);
      // write it to the ini file
      AppIni.WriteString(INI_SEC_MIDI, inikey, lb.items[ix] );
    end;

  end else begin // close
    device.CloseAll;
    lb.ItemIndex := -1;
    AppIni.DeleteKey(INI_SEC_MIDI, inikey);
  end;

  EnableDisableControls;
end;


{%region MsgCombobox}
procedure TMainForm.ReloadMsgCombo;
begin
  msgdb.LoadMsgKeys(SaveMsgCombobox.Items);
end;


procedure TMainForm.SaveMsgButtonClick(Sender: TObject);
var
  key, s : string;
begin
  key := trim(SaveMsgComboBox.Text);
  s := trim(MidiSendMemo.Text);
  if (length(key) = 0) or (length(s) = 0) then
    exit;

  msgdb.SaveMsg(key, s);
  ReloadMsgCombo;
end;


procedure TMainForm.SaveMsgComboBoxSelect(Sender: TObject);
var
  key, s : string;
begin
  key := trim(SaveMsgComboBox.Text);
  if length(key) = 0 then
    exit;
  s := msgdb.ReadMsg(key);
  MidiSendMemo.Text := s;
end;

procedure TMainForm.SysexDelayTrackBarChange(Sender: TObject);
begin
  SysExDelayLabel.Caption := inttostr(SysexDelayTrackBar.Position);
end;

procedure TMainForm.SysexDelayTrackBarExit(Sender: TObject);
begin
  if SysexDelayTrackBar.Position <> MsToWaitBetweenSysex then begin
    MsToWaitBetweenSysex := SysexDelayTrackBar.Position;
    AppIni.WriteInt64(INI_SEC_MIDI, INI_SYSEX_DELAY, MsToWaitBetweenSysex);
  end;
end;


procedure TMainForm.DeleteMsgButtonClick(Sender: TObject);
var
  key : string;
begin
  key := trim(SaveMsgComboBox.Text);
  if length(key) = 0 then
    exit;

  msgdb.DeleteMsg(key);
  ReloadMsgCombo;
end;

{%endregion MsgCombobox%}


function TMainForm.GetMidiInDevice(showerr : boolean = false) : integer;
begin
  if MidiInCheckBox.Checked then
    result := MidiInListBox.ItemIndex
  else
    result := -1;

  if showerr and (result = -1) then
    showmessage('No MIDI In device selected');
end;


function TMainForm.GetMidiOutDevice(showerr : boolean = false) : integer;
begin
  if MidiOutCheckBox.Checked then
    result := MidiOutListBox.ItemIndex
  else
    result := -1;

  if showerr and (result = -1) then
    showmessage('No MIDI Out device selected');
end;


procedure TMainForm.EnableDisableControls;
begin
  MidiInListbox.Enabled := not MidiInCheckBox.Checked;
  MidiOutListbox.Enabled := not MidiOutCheckBox.Checked;
end;

{ --------------------- MIDI Functionality -----------------------------------}
{%region MIDI Events}
procedure TMainForm.OnSysExDataCALLBACK( const aDeviceIndex: integer; const aStream: TMemoryStream);
begin
  // NOTE: Don't execute GUI code inside a CALLBACK!!!
  if aStream.Size = 0 then
    exit;

  fSysExBufferStr := SysExStreamToStr(aStream);

  PostMessage( Application.MainForm.Handle,
               WM_MIDISYSEX_ARRIVED,
               aDeviceIndex,
               0 );
end;

procedure TMainForm.WmSysexDataArrived( var Msg: TMessage ); // message WM_SYSEXDATA_ARRIVED;
begin
  if length(fSysExBufferStr) = 0 then
    exit;

  SysExMemo.Append(fSysExBufferStr);
  fSysExBufferStr := '';
end;


procedure TMainForm.OnMidiInDataCALLBACK( const aDeviceIndex: integer; const aStatus, aData1, aData2: byte);
begin
{ dms - Should we echo midi in to midi out????
        Maybe check to see if midi in device name <> midi out device name?
  if (aStatus <> $F8)     // IGNORE real-time message clock $F8 = 248
    and (aStatus <> $FE)  // IGNORE "Active Sensing" $FE = 254
    then MidiOutput.Send( aDeviceIndex +1, aStatus, aData1, aData2);
}

  // post the MIDI data. No gui allowed in CALLBACK
  PostMessage( Application.MainForm.Handle,
               WM_MIDIDATA_ARRIVED,
               aDeviceIndex,
               aStatus + (aData1 and $FF) shl 8 + (aData2 and $FF) shl 16
               );
end;


procedure TMainForm.WmMidiDataArrived(var Msg: TMessage); // message WM_MIDIDATA_ARRIVED;
begin
  // simply display the values:
  if (Msg.lParamlo <> $F8)     // IGNORE real-time tempo message clock $F8 = 248
  and (Msg.lParamlo <> $FE)    // IGNORE "Active Sensing" $FE = 254
  then begin
    MidiMsgMemo.Append( MidiMsgToStr(
      Msg.lParamlo and $FF,
      (Msg.lParamlo shr 8) and $FF,
      Msg.lParamhi and $FF
      ));
  end;
end;


procedure TMainForm.WmMidiInError(var Msg: TMessage); // message WM_MIM_ERROR;
begin
  DebugMemo.Append( 'MIDI Error: ' + inttostr(Msg.lParam) );
end;


procedure TMainForm.WmLongError( var Msg: TMessage);
begin
  sysexmemo.Append( 'Long Error: ' + inttostr(Msg.lParam) );
end;
{%endregion MIDI Events}


end.

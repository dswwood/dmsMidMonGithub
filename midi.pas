unit MIDI;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}


{ ***   You can choose if You want MIDI.PAS to use exceptions    *** }
{ ***   in case of Errors. If not, MIDI.PAS uses POSTMESSAGE()   *** }
{ ***   and You have to handle these messages in Your MainForm   *** }

//{$define MIDI_UseExceptions}
//{$define DebugSysEx}

{$DEFINE DebugMidiMsg}
{ DMS - When enabled, this enables logging debug messages to a TStrings object,
  such as tmemo.lines or TStringList.
  To enable, set var midi_debug_strs to the TStrings you want to use as a log.
  i.e. midi_debug_strs := Memo1.Lines
}

{*******************************************************************************
*                                                                              *
*                                MIDI.PAS                                      *
*                                                                              *
*     This file is based on the MIDI device classes by Adrian Meyer            *
*     This file was taken from the ZIP archive 'demo_MidiDevices_D6.zip'       *
*     and partly changed by me, some changes take over from  'DAV_MidiIO.pas'  *
*                                                                              *
*                       changes 2020-12-26                                     *
*                   DMS changes 2025-03-20                                     *
********************************************************************************
* V1.0  First release with simple MIDI Input/Output                            *
* V1.1  SysEx Input Event added, refactured error handling                     *
* V1.2  SysEx Output procedure added, changes sysex input for multiple ports   *
* V1.3  Changes by BREAKOUTBOX 2009-07  (www.breakoutbox.de)                   *
* V1.4  Changes adapted from DAV_MidiIO.pas - see http://www.ohloh.net/p/DAV   *
* V1.5  removed an Exception on sending SysEx Data to a CLOSED Output          *
* V1.6  added a Switch to choose between Exceptions and Other behaviore ...    *
* V1.7  replaced  pChar => pAnsiChar  Char => AnsiChar  string => AnsiString   *
*       to gain compatibility to DelphiXE and higher versions ..               *
********************************************************************************
* 2020-12-24  bugfix for 64 bit Lazarus - thanks to J.M.                       *
* 2020-12-25  reformatting, deactivate PostMessage() which was a mess          *
* 2020-12-26  change and edit some comments about how this unit works ..       *
********************************************************************************
* 2025-01-20  DMS - Made several modifications. Added $DEFINE DebugMidiMsg   *
*       -     Added $DEFINE DebugMidiMsg                                                                 *
*       -                                                                      *
*       -                                                                      *
*       -                                                                      *
*******************************************************************************}


{ ---------------------------------------------------------------------------- }
interface
{ ---------------------------------------------------------------------------- }

uses
  {$ifdef FPC}
  Interfaces,
  JWAwindows,
  {$else}
  Windows,
  {$endif}
  Forms, Classes, Messages, SysUtils, Math, Contnrs, MMSystem, Dialogs;


// -------------------------- WARNING --------------------------
// If Your Application uses User-defined Messages YOU MUST CHECK
// if these values collide with one of Your own messages !!!
const
  // define window messages for MainForm :
  WM_MIDISYSTEM_MESSAGE = WM_USER +1;
  WM_MIDIDATA_ARRIVED   = WM_USER +2;
  WM_MIM_ERROR          = WM_USER +3;//1001;
  WM_MIM_LONGERROR      = WM_USER +4;//1002;
  WM_MIDISYSEX_ARRIVED  = WM_USER +5;  // added by dms


const
  // define Your size of  System Exclusive buffer :
  cMySysExBufferSize = 2048;


type
  TMIDIChannel      = 1..16;
  TMIDIDataByte     = 0..$7F;           //  7 bits
  TMIDIDataWord     = 0..$3FFF;         // 14 bits
  TMIDIStatusByte   = $80..$FF;
  TMIDIVelocity     = TMIDIDataByte;
  TMIDIKey          = TMIDIDataByte;
  TMIDINote         = TMIDIKey;

type
  // EVENT if data is received - ATTENTION - This is code inside a CALLBACK PROCEDURE !!!
  TOnMidiInData = procedure( const aDeviceIndex: integer; const aStatus, aData1, aData2: byte) of object;
  // EVENT if system exclusive data is received
  TOnSysExData = procedure( const aDeviceIndex: integer; const aStream: TMemoryStream) of object;

  EMidiDevices = Exception;

  // base class for MIDI devices
  TMidiDevices = class
  private
    fDevices    : TStringList;
    fMidiResult : MMResult;
    procedure SetMidiResult(const Value: MMResult);
  protected
    property MidiResult: MMResult read fMidiResult write SetMidiResult;
    function GetHandle(const aDeviceIndex: integer): THandle;
  public
    // create the MIDI devices
    constructor Create; virtual;
    // whack the devices
    destructor Destroy; override;
    // open a specific device
    procedure Open(const aDeviceIndex: integer); virtual; abstract;
    // close a specific device
    procedure Close(const aDeviceIndex: integer); virtual; abstract;
    // close all devices
    procedure CloseAll;
    // THE devices
    function IsOpen(ADeviceIndex: Integer): Boolean;
    property Devices: TStringList read fDevices;
  end;

  // MIDI input devices
  TMidiInput = class(TMidiDevices)
  private
    // event if data is received - ATTENTION - This code is executed inside a CALLBACK PROCEDURE !!!
    fOnMidiData  : TOnMidiInData;
    fOnSysExData : TOnSysExData;
    fSysExData   : TObjectList;
  protected
    procedure DoSysExData( const aDeviceIndex: integer);
  public
    // create an input device
    constructor Create; override;
    // what the input devices
    destructor Destroy; override;
    // open a specific input device
    procedure Open( const aDeviceIndex: integer); override;
    // close a specific device
    procedure Close( const aDeviceIndex: integer); override;
    // midi data event - ATTENTION - This code is executed inside a CALLBACK PROCEDURE !!!
    property OnMidiData: TOnMidiInData read fOnMidiData write fOnMidiData;
    // midi system exclusive is received
    property OnSysExData: TOnSysExData read fOnSysExData write fOnSysExData;
  end;

  // MIDI output devices
  TMidiOutput = class(TMidiDevices)
    constructor Create; override;
    // open a specific input device
    procedure Open(const aDeviceIndex: integer); override;
    // close a specific device
    procedure Close(const aDeviceIndex: integer); override;
    // send some midi data to the indexed device
    procedure Send(const aDeviceIndex: integer; const aStatus, aData1, aData2: byte);
    procedure SendSystemReset( const aDeviceIndex: integer);
    procedure SendAllSoundOff( const aDeviceIndex: integer; const channel: byte);
    // send system exclusive data to a device
    procedure SendSysEx( const aDeviceIndex: integer; const aStream: TMemoryStream); overload;
    procedure SendSysEx( const aDeviceIndex: integer; const aBytes: TBytes); overload; // added by dms
    procedure SendSysEx( const aDeviceIndex: integer; const aString: AnsiString); overload;
  end;

  // convert the stream into xx xx xx xx AnsiString
  function SysExStreamToStr( const aStream: TMemoryStream): AnsiString;
  // fill the AnsiString in a xx xx xx xx into the stream  
  procedure StrToSysExStream( const aString: AnsiString; const aStream: TMemoryStream);


  // access MIDI input devices - see local VAR gMidiInput in implementation section
  function MidiInput: TMidiInput;
  // access MIDI output devices - see local VAR gMidiOutput in implementation section
  function MidiOutput: TMidiOutput;


  type
    TSysExBuffer = array[0..cMySysExBufferSize -1] of AnsiChar;

    TSysExData = class
    private
      fSysExStream: TMemoryStream;
    public
      SysExHeader: {$ifdef FPC} _midihdr {$else} TMidiHdr {$endif};
      SysExData: TSysExBuffer;
      constructor Create;
      destructor Destroy; override;
      property   SysExStream: TMemoryStream read fSysExStream;
    end;

{ set this to a tmemo.lines or whatever to log debug messages.
  Only used if DebugMidiMsg is defined. }
var
  midi_debug_strs : TStrings;

{ ---------------------------------------------------------------------------- }
implementation
{ ---------------------------------------------------------------------------- }

{$IFDEF DebugMidiMsg}  // Added by dms
  procedure DebugMsg( const s : string );
  begin
    if assigned(midi_debug_strs) then
      midi_debug_strs.add('DebugMidiMsg: ' + s);
  end;
{$ENDIF DebugMidiMsg}

var
  gMidiInput: TMidiInput;
  gMidiOutput: TMidiOutput;



// The functions  MidiInput and  MidiOutput  do "autocreate" the MIDI objects :
// ----------------------------------------------------------------------------
//
// On first call they check if  gMidiInput  and  gMidiOutput  do exist at all.
// If not, these functions do create these Objects.
//
// This only works flawlessly because in initialization section (see end of file !)
// both gMidiInput  and  gMidiOutput  are set to NIL !
//
// So it is possible to write for example   MidiInput.Open() .. and it works !
//
//
// When Your application terminates, somewhere after  procedure TForm1.FormClose(),
// then in the finalization section (see end of file !)
// the midi.pas unit terminates these objects with
//   FreeAndNil( gMidiInput);
//   FreeAndNil( gMidiOutput);
//
// So You never have to  CREATE or TERMINATE  these objects in Your application ..
// It is done automatically.
//
//
// If this is "good code" quality .. this is a different question.
// The disadvantage is:  You have no control over
// - when are the MIDI objects created ?
// - when are the MIDI objects destroyed ?
//
// So when facing problems You should probably CREATE and DESTROY Your own Objects.
// For example define  "MyMidiInput:TMidiInput"  and  "MyMidiInput:TMidiInput"
// in Your own application ..

function MidiInput: TMidiInput;
begin
  if not Assigned( gMidiInput)
    then gMidiInput := TMidiInput.Create;
  Result := gMidiInput;
end;

function MidiOutput: TMidiOutput;
begin
  if not Assigned( gMidiOutput)
    then gMidiOutput := TMidiOutput.Create;
  Result := gMidiOutput;
end;


{$IFDEF FPC}
//type
//  PHMIDIIN = ^HMIDIIN;
//  TMidiOutCaps = TMidiOutCapsA;
{$ENDIF}

{ I don't know whatfor this RECORD is used in DAV_MidiIO.pas
  but I think this maybe allows renumeration / resorting of MIDI devices
  for use in Your application - I didn't use this until now
  so I didn't take over the implementation in this MIDI.PAS ... !

TMidiInputDeviceRecord = record
    MidiInput    : TMidiInput;
    DeviceNumber : Integer;
  end;
  PMidiInputDeviceRecord = ^TMidiInputDeviceRecord;
}


{ The Callback-Procedure receives MIDI data on interrupt }
procedure MidiInCallback( aMidiInHandle: PHMIDIIN; aMsg: Integer; aInstance,
                          aMidiData, aTimeStamp: integer); stdcall;
begin
  case aMsg of
    MIM_DATA:
      begin
        if Assigned( MidiInput.OnMidiData) then
          begin
            // This is the method with a "Procedure Of Object":
            MidiInput.OnMidiData(  aInstance,
                                   aMidiData and $000000FF,
                                  (aMidiData and $0000FF00) shr 8,
                                  (aMidiData and $00FF0000) shr 16);

          end;
        // This would be the classic method with a "windows message":
        // PostMessage( Application.MainForm.Handle, WM_MIDIDATA_ARRIVED, aInstance, aMidiData);
      end;

    MIM_LONGDATA:
      MidiInput.DoSysExData( aInstance);

    MIM_ERROR:
      PostMessage(Application.MainForm.Handle, WM_MIM_ERROR, aInstance, aMidiData);

    MIM_LONGERROR:
      PostMessage(Application.MainForm.Handle, WM_MIM_LONGERROR, aInstance, aMidiData);
  end;
end;


{ ***** TMidiDevices ********************************************************* }
constructor TMidiDevices.Create;
begin
  FDevices:= TStringList.Create;
end;

destructor TMidiDevices.Destroy;
begin
  FreeAndNil( FDevices);
  inherited;
end;


{ ***** TMidiInput *********************************************************** }
constructor TMidiInput.Create;
var
  AvailableMIDIinputs : integer;
  lInCaps             : TMidiInCaps;
  i                   : integer;
begin
  inherited;
  fSysExData := TObjectList.Create(true);
  TRY   // TRY..EXCEPT was adapted from file "DAV_MidiIO.pas"
    AvailableMIDIinputs:= MidiInGetNumDevs;
  EXCEPT
    AvailableMIDIinputs:= 0;
  end;

  if AvailableMIDIinputs > 0 then
  for i:= 0 to AvailableMIDIinputs - 1 do begin
    MidiResult := midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));
    if MidiResult = MMSYSERR_NOERROR then begin
      fDevices.Add(StrPas(lInCaps.szPname));
      fSysExData.Add(TSysExData.Create);
    end;
  end;
end;

destructor TMidiInput.Destroy;
begin
  FreeAndNil(fSysExData);
  inherited;
end;


procedure TMidiInput.Close(const aDeviceIndex: integer);
var
  aHandle : THandle;
begin
  aHandle := GetHandle(aDeviceIndex);
  if aHandle <> 0 then begin
    MidiResult := midiInStop(aHandle);
    MidiResult := midiInReset(aHandle);
    MidiResult := midiInUnprepareHeader(aHandle,
                  @TSysExData(fSysExData[aDeviceIndex]).SysExHeader, SizeOf(TMidiHdr));
    MidiResult := midiInClose(aHandle);
    FDevices.Objects[aDeviceIndex] := nil;
  end;
end;


procedure TMidiDevices.CloseAll;
var
  i : integer;
begin
  for i := 0 to FDevices.Count - 1 do
    Close(i);
end;


procedure TMidiInput.Open( const aDeviceIndex: integer);
var
  lSysExData : TSysExData;
  lHandle    : THandle;
begin
  if GetHandle( aDeviceIndex ) <> 0 then
    exit;

  // changed from cardinal to PtrUInt 2020-12-24 - thanks to J.M.
  MidiResult := midiInOpen( @lHandle, aDeviceIndex, {%H-}PtrUInt(@midiInCallback),
                            aDeviceIndex, CALLBACK_FUNCTION );

  fDevices.Objects[ aDeviceIndex ] := TObject( lHandle );
  lSysExData := TSysExData( fSysExData[aDeviceIndex] );

  lSysExData.SysExHeader.dwFlags := 0;

  // DRAGONS:  why are the function returns not checked on errors here ?
  MidiResult := midiInPrepareHeader( lHandle, @lSysExData.SysExHeader, SizeOf(TMidiHdr) );
  MidiResult := midiInAddBuffer( lHandle, @lSysExData.SysExHeader, SizeOf(TMidiHdr) );
  MidiResult := midiInStart(lHandle);
end;


{ ***** TMidiInput - SysEx *************************************************** }
procedure TMidiInput.DoSysExData( const aDeviceIndex: integer );
var
  lSysExData : TSysExData;
begin
  lSysExData := TSysExData(fSysExData[aDeviceIndex]);
  if lSysExData.SysExHeader.dwBytesRecorded = 0
    then Exit;

  lSysExData.SysExStream.Write( lSysExData.SysExData,
                                lSysExData.SysExHeader.dwBytesRecorded);
  if lSysExData.SysExHeader.dwFlags and MHDR_DONE = MHDR_DONE then begin
    lSysExData.SysExStream.Position := 0;
    if assigned(fOnSysExData) then
      fOnSysExData(aDeviceIndex, lSysExData.SysExStream);
    lSysExData.SysExStream.Clear;
  end;

  lSysExData.SysExHeader.dwBytesRecorded := 0;

  // DRAGONS:  why not check function returns on errors here ?
  MidiResult := MidiInPrepareHeader( GetHandle(aDeviceIndex),
                                     @lSysExData.SysExHeader,
                                     SizeOf(TMidiHdr));
  MidiResult := MidiInAddBuffer( GetHandle(aDeviceIndex),
                                 @lSysExData.SysExHeader,
                                 SizeOf( TMidiHdr));
end;


{ ***** TMidiOutput ********************************************************** }
constructor TMidiOutput.Create;
var
  AvailableMIDIoutputs : integer;
  lOutCaps             : TMidiOutCaps;
  i                    : integer;
begin
  inherited;

  TRY
    AvailableMIDIoutputs := MidiOutGetNumDevs;
  EXCEPT
    AvailableMIDIoutputs := 0;
  end;

  //ShowMessage( 'DEBUG - AvailableMIDIoutputs = ' +IntToStr( AvailableMIDIoutputs));
  for i:= 0 to AvailableMIDIoutputs - 1 do begin
    MidiResult := MidiOutGetDevCaps( i, @lOutCaps, SizeOf(TMidiOutCaps));
    fDevices.Add( lOutCaps.szPname);
  end;
end;


procedure TMidiOutput.Open( const aDeviceIndex: integer);
var
  lHandle: THandle;
begin
  {$ifndef FPC}
  inherited;  // Lazarus doesn't like this, so for Delphi only ..
  {$endif}
  // device already open;
  if GetHandle(aDeviceIndex) <> 0 then
    Exit;

  MidiResult := midiOutOpen( @lHandle, aDeviceIndex, 0, 0, CALLBACK_NULL);
  fDevices.Objects[ aDeviceIndex ]:= TObject( lHandle);
end;


procedure TMidiOutput.Close( const aDeviceIndex: integer);
begin
  {$ifndef FPC}
  inherited;  // Lazarus doesn't like this, so for Delphi only ..
  {$endif}
  if GetHandle(aDeviceIndex) <> 0 then begin // 'if .. then' added by BREAKOUTBOX 2009-07-15
    MidiResult := midiOutClose(GetHandle(aDeviceIndex));
    fDevices.Objects[ aDeviceIndex ] := nil;
  end;
end;


procedure TMidiOutput.Send( const aDeviceIndex: integer; const aStatus,
                            aData1, aData2: byte);
var
  lMsg: cardinal;
begin
  {$IFDEF DebugMidiMsg}
  DebugMsg( 'TMidiOutput.Send- Device:' + inttostr(aDeviceIndex) +
            ' status:' + hexstr(aStatus,2) +
            ' aData1:' + hexstr(aData1,2) +
            ' aData2:' + hexstr(aData2,2)
  );
  {$ENDIF}

  // open if the device is not open      // NOW: do NOT open .. !
  if not Assigned( fDevices.Objects[aDeviceIndex] ) then
    raise Exception.Create('TMidiOutput.Send: Device is not open:' + inttostr(aDeviceIndex));

  lMsg := aStatus or (aData1 shl 8) or (aData2 shl 16); // better ?
  MidiResult := MidiOutShortMsg(GetHandle(aDeviceIndex), lMsg);

  {$IFDEF DebugMidiMsg}
  if MidiResult <> 0 then
    DebugMsg( 'TMidiOutput.Send- MidiResult:' + inttostr(MidiResult) );
  {$ENDIF}
end;


{ --- common MIDI Out messages ----------------------------------------------- }
{ System Reset = Status Byte FFh }
procedure TMidiOutput.SendSystemReset( const aDeviceIndex: integer);
begin
  Self.Send( aDeviceIndex, $FF, $0, $0);
end;


{ All Sound Off = Status + Channel Byte Bnh, n = Channel number  }
{                 Controller-ID = Byte 78h,  2nd Data-Byte = 00h }
procedure TMidiOutput.SendAllSoundOff( const aDeviceIndex: integer; const channel: byte);
begin
  Self.Send( aDeviceIndex, $b0 +channel, $78, $0);
end;


// HINT:  in a Thread MidiInGetErrorText() makes no sense ..
//        read out the error text in Your main thread / MainForm !
procedure TMidiDevices.SetMidiResult( const Value: MMResult);
var
  lError: array[0..MAXERRORLENGTH] of AnsiChar;
begin
  fMidiResult := Value;
  if fMidiResult <> MMSYSERR_NOERROR then
    if MidiInGetErrorText( fMidiResult, @lError, MAXERRORLENGTH) = MMSYSERR_NOERROR then
      {$ifdef MIDI_UseExceptions}
      raise EMidiDevices.Create(StrPas(lError));
      {$else}
      PostMessage( Application.MainForm.Handle, WM_MIDISYSTEM_MESSAGE, fMidiResult, 0);
      {$endif}
end;


function TMidiDevices.GetHandle(const aDeviceIndex: integer): THandle;
begin
  if not InRange(aDeviceIndex, 0, fDevices.Count - 1) then
    raise ERangeError.CreateFmt( '%s: Device index out of bounds! (%d)',
                                  [ClassName, aDeviceIndex] );
  try
    Result := THandle(fDevices.Objects[ aDeviceIndex ]);
  except
    Result:= 0;
  end;
end;


function TMidiDevices.IsOpen(ADeviceIndex: Integer): boolean;
begin
  Result := GetHandle(ADeviceIndex) <> 0;
end;


{ ***** TMidiOutput - SysEx ************************************************** }
procedure TMidiOutput.SendSysEx( const aDeviceIndex: integer;
                                 const aString: AnsiString);
var
  lStream: TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    StrToSysExStream( aString, lStream );
    SendSysEx( aDeviceIndex, lStream );
  finally
    FreeAndNil( lStream );
  end;
end;


procedure TMidiOutput.SendSysEx( const aDeviceIndex: integer;
                                 const aStream: TMemoryStream);
var
  lSysExHeader: TMidiHdr;
  aHandle : THandle;
begin
  // dms - changed this to raise exception if deviceindex is not in range
  //       or if the device is not open.
  aHandle := GetHandle(aDeviceIndex); // this will raise an exception if out of range
  if aHandle = 0 then
    raise Exception.create('SendSysEx: device is not open.');

  aStream.Position := 0;
  lSysExHeader.dwBufferLength := aStream.Size;
  lSysExHeader.lpData := aStream.Memory;
  lSysExHeader.dwFlags := 0;

  //ShowMessage( 'HEX: ' +SysExStreamToStr( aStream));
  {$ifdef DebugSysEx}  ShowMessage( '0 - ' +IntToStr(MidiResult));  {$endif}
  MidiResult := midiOutPrepareHeader(aHandle, @lSysExHeader, SizeOf(TMidiHdr));
  {$ifdef DebugSysEx}  ShowMessage( '1 - ' +IntToStr(MidiResult));  {$endif}
  MidiResult := midiOutLongMsg(aHandle, @lSysExHeader, SizeOf(TMidiHdr));
  {$ifdef DebugSysEx}  ShowMessage( '2 - ' +IntToStr(MidiResult));  {$endif}
  MidiResult := midiOutUnprepareHeader(aHandle, @lSysExHeader, SizeOf(TMidiHdr));
  {$ifdef DebugSysEx}  ShowMessage( '3 - ' +IntToStr(MidiResult));  {$endif}
  {$ifdef DebugSysEx}  ShowMessage( '4 - ' +aStream.ReadAnsiString);  {$endif}
end;


// Added by dms: Overloaded to allow passing a byte array.
procedure TMidiOutput.SendSysEx( const aDeviceIndex: integer; const aBytes: TBytes);
var
  ms : TMemoryStream;
begin
  ms := TMemoryStream.create;
  try
    ms.Write( aBytes[0], Length(aBytes) );
    SendSysEx( aDeviceIndex, ms );
  finally
    ms.free;
  end;
end;


{ ***** TSysExData *********************************************************** }
constructor TSysExData.Create;
begin
  SysExHeader.dwBufferLength := cMySysExBufferSize;
  SysExHeader.lpData := SysExData;
  fSysExStream := TMemoryStream.Create;
end;


destructor TSysExData.Destroy;
begin
  FreeAndNil( fSysExStream);
end;


{ ***** Helper Functions ***************************************************** }
function SysExStreamToStr(const aStream: TMemoryStream): AnsiString;
var
  i : integer;
begin
  Result := '';
  for i:= 0 to aStream.Size - 1 do
    Result := Result + Format( '%.2x ', [ byte(pAnsiChar(aStream.Memory)[i]) ]);

  // dms- get rid of the trailing space, if any.
  i := length(Result);
  if i > 0 then
    SetLength(Result, i-1);
end;


//
// DMS: Added more validation and more descriptive error messages.
//
// This procedure accepts a sysex hex string like 'F0 aa bb cc dd F7' or 'F0aabbccddF7'
//
procedure StrToSysExStream(const aString: AnsiString; const aStream: TMemoryStream);
const
  cHex : AnsiString = '123456789ABCDEF';
var
  lStr : AnsiString;
  i    : integer;
  L    : integer;
begin
  // check on errors  - added by BREAKOUTBOX 2009-07-30
  lStr := StringReplace( AnsiUpperCase(aString), ' ', '', [rfReplaceAll]);

  L := length( lstr );
  if not (L mod 2 = 0) then // as HEX every byte must be two AnsiChars long, for example '0F'
    raise EConvertError.Create( 'SysEx hex string must have 2 chars per byte: ' + aString);

  if L < 10 then // shortest System Exclusive Message = 5 bytes = 10 hex AnsiChars
    raise EConvertError.Create( 'SysEx string too short. It must have at least 5 bytes: ' + aString);

  // make sure string has only hex chars.
  for i := 1 to length(lStr) do
    if not (lstr[i] in ['0'..'9', 'A'..'F']) then
      raise EConvertError.Create( 'SysEx string may only contain hex digits (0..9, A..F): ' + aString);

  aStream.Size := L div 2; // ' - 1' removed by BREAKOUTBOX 2009-07-15
  aStream.Position := 0;

  for i:= 1 to aStream.Size do
    pAnsiChar(aStream.Memory)[i-1] :=
      AnsiChar( AnsiPos(lStr[ i*2 - 1], cHex) shl 4 + AnsiPos(lStr[i*2], cHex) );
end;



initialization
  gMidiInput  := nil;
  gMidiOutput := nil;

finalization
  FreeAndNil( gMidiInput);
  FreeAndNil( gMidiOutput);


end.

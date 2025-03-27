unit dmsMidiUtil;

{$mode ObjFPC}{$H+}

{$inline on}

{ ---------------------------------------------------------------------------- }
                                   interface
{ ---------------------------------------------------------------------------- }

uses
  Classes, SysUtils;

type
  // Makes it easier to access a DWord as TBytes or vice versa.
  TDWord = record
    case integer of
      1 : (DWord4 : DWORD);                   // as 4 byte word
      2 : (Bytes  : Array [0..3] of Byte);    // as bytes
  end;


// Dump dw values to TStrings for debugging. (i.e. Tstrings - TMemo.Lines())
procedure DumpTDword( dw : TDword; sl : TStrings );

function HiNibble( b : byte ) : byte; inline;
function LoNibble( b : byte ) : byte; inline;

function ConvertNumber(const numStr: string): string;

function HexStringToBytes(const HexString: string): TBytes;
function BytesToHexString(const bytes : TBytes): string;

function BytesToAscii( bytes : TBytes ) : string;
function AsciiStrToHexStr(const astr : string) : string;

function RolandChecksum(const Data: array of Byte): Byte;

// Parses a hex byte string for [brackets] and inserts a checksum byte after ']'
// i.e. converts "F0 [11 22 33 44] F7" to "F0 11 22 33 44 nn F7"
function ParseRolandChecksum(const hstr : string): string;

type
  TShortMidiMsgRecord = record
    case Integer of
      0: (
        aStatus: Byte;
        aData1: Byte;
        aData2: Byte;
      );
      1: (
        DataArray: array[0..2] of Byte;
      );
  end;

function StrToShortMidiMsg( const aString : string ) : TShortMidiMsgRecord;

function MidiMsgToStr( aStatus, aData1, aData2: Byte) : string;

{ ---------------------------------------------------------------------------- }
                             implementation
{ ---------------------------------------------------------------------------- }

uses
  strutils;

function MidiMsgToStr(aStatus, aData1, aData2: Byte): String;
var
  statusHex, data1Hex, data2Hex: String;
  messageType: String;
  CCMsgStr: String;
begin
  // Convert bytes to hex strings
  statusHex := IntToHex(aStatus, 2);
  data1Hex := IntToHex(aData1, 2);
  data2Hex := IntToHex(aData2, 2);

  // Determine the MIDI message type based on the status byte
  case (aStatus and $F0) of
    $80: messageType := 'Note Off';
    $90: messageType := 'Note On';
    $A0: messageType := 'Polyphonic Key Pressure (Aftertouch)';
    $B0:
    begin
      messageType := 'Control Change';
      case aData1 of
        $00: CCMsgStr := 'Bank Select';
        $01: CCMsgStr := 'Modulation Wheel or LFO';
        $02: CCMsgStr := 'Breath Controller';
        $04: CCMsgStr := 'Foot Controller';
        $05: CCMsgStr := 'Portamento Time';
        $06: CCMsgStr := 'Data Entry MSB';
        $07: CCMsgStr := 'Channel Volume';
        $08: CCMsgStr := 'Balance';
        $0A: CCMsgStr := 'Pan';
        $0B: CCMsgStr := 'Expression Controller';
        $0C: CCMsgStr := 'Effect Control 1';
        $0D: CCMsgStr := 'Effect Control 2';
        $10..$13: CCMsgStr := 'General Purpose Controller 1-4';
        $20..$3F: CCMsgStr := 'LSB for Controllers 0-31';
        $40: CCMsgStr := 'Damper Pedal (Sustain)';
        $41: CCMsgStr := 'Portamento On/Off';
        $42: CCMsgStr := 'Sostenuto On/Off';
        $43: CCMsgStr := 'Soft Pedal On/Off';
        $44: CCMsgStr := 'Legato Footswitch';
        $45: CCMsgStr := 'Hold 2';
        $46..$4F: CCMsgStr := 'Sound Controller 1-10';
        $50..$53: CCMsgStr := 'General Purpose Controller 5-8';
        $54..$5A: CCMsgStr := 'Portamento Control'; //Spec says reserved, but many synths use it for this
        $5B: CCMsgStr := 'Effects Level';
        $5C: CCMsgStr := 'Tremolo Depth';
        $5D: CCMsgStr := 'Chorus Depth';
        $5E: CCMsgStr := 'Celeste Depth';
        $5F: CCMsgStr := 'Phaser Depth';
        $60: CCMsgStr := 'Data Increment';
        $61: CCMsgStr := 'Data Decrement';
        $62: CCMsgStr := 'Non-Registered Parameter Number LSB';
        $63: CCMsgStr := 'Non-Registered Parameter Number MSB';
        $64: CCMsgStr := 'Registered Parameter Number LSB';
        $65: CCMsgStr := 'Registered Parameter Number MSB';
        $66..$77: CCMsgStr := 'Reserved';
        $78: CCMsgStr := 'All Sound Off';
        $79: CCMsgStr := 'Reset All Controllers';
        $7A: CCMsgStr := 'Local Control On/Off';
        $7B: CCMsgStr := 'All Notes Off';
        $7C: CCMsgStr := 'Omni Mode Off';
        $7D: CCMsgStr := 'Omni Mode On';
        $7E: CCMsgStr := 'Mono Mode On';
        $7F: CCMsgStr := 'Poly Mode On';
      else
        CCMsgStr := 'Undefined Control Change';
      end;
    end;
    $C0: messageType := 'Program Change';
    $D0: messageType := 'Channel Pressure (Aftertouch)';
    $E0: messageType := 'Pitch Bend';

    $F1 : messageType := 'MIDI Time Code Quarter Frame';
    $F2 : messageType := 'Song Position Pointer: LSB:MSB';
    $F3 : messageType := 'Song Select: Song Number';
    $F6 : messageType := 'Tune Request';

    $F8 : messageType := 'Timing Clock';
    $FA : messageType := 'Start';
    $FB : messageType := 'Continue';
    $FC : messageType := 'Stop';
    $FE : messageType := 'Active Sensing';
    $FF : messageType := 'System Reset';

    else messageType := 'Unknown';
  end;

  result := Format('%s %s %s - %s', [statusHex, data1Hex, data2Hex, messageType]);
  if messageType = 'Control Change' then
    result := Format('%s: %s', [result, CCMsgStr]);
end;


procedure DumpTDword( dw : TDword; sl : TStrings );
var
  dw2 : TDWord;
begin
  sl.append( 'Conversion' );
  sl.append( 'DWord     : ' + IntToHex(dw.DWord4, 8) + 'h, ' + inttostr(dw.dword4) + 'd');
  sl.append( 'Bytes     : ' +
    IntToHex(dw.Bytes[0], 2) + ' ' + IntToHex(dw.Bytes[1], 2) + ' ' +
    IntToHex(dw.Bytes[2], 2) + ' ' + IntToHex(dw.Bytes[3], 2) + ', (' +
    inttostr(dw.Bytes[0]) + 'd ' + inttostr(dw.Bytes[1]) + 'd ' +
    inttostr(dw.Bytes[2]) + 'd ' + inttostr(dw.Bytes[3]) + 'd)' );

  dw2.DWord4 := SwapEndian(dw.DWord4);
  sl.append( 'SwapEndian: ' +
    IntToHex(dw2.Bytes[0], 2) + ' ' + IntToHex(dw2.Bytes[1], 2) + ' ' +
    IntToHex(dw2.Bytes[2], 2) + ' ' + IntToHex(dw2.Bytes[3], 2) + ', (' +
    inttostr(dw2.Bytes[0]) + 'd ' + inttostr(dw2.Bytes[1]) + 'd ' +
    inttostr(dw2.Bytes[2]) + 'd ' + inttostr(dw2.Bytes[3]) + 'd)' );
end;


function HiNibble( b : byte ) : byte; inline;
begin
  result := b shr 4;  // 11110000 -> 00001111
end;

function LoNibble( b : byte ) : byte; inline;
begin
  result := b and %00001111 // $0F;
end;

{ Converts a hexadecimal string to a TBytes array.

  @param HexString The hexadecimal string to convert.
  @returns A TBytes array containing the converted bytes.
  @raise Exception If the input string is not a valid hex string.
}
function HexStringToBytes(const HexString: string): TBytes;
var
  i, ix : Integer;
  s : string;
begin
  result := nil;

  // clean the string
  s := StringReplace( AnsiUpperCase(HexString), ' ', '', [rfReplaceAll] );

  // Input validation: Check for even length and valid hex characters.
  if Length(s) mod 2 <> 0 then
    raise Exception.Create('HexString must have an even number of characters: ' + HexString);

  SetLength(Result, Length(s) div 2);
  ix := 0;
  for i := 1 to length(s) do begin
    if odd(i) then try
      // Convert two hex digits to a byte.
      Result[ix] := StrToInt('$' + Copy(s, i, 2));
      inc(ix);
    except
      on EConvertError do
        raise Exception.Create('Invalid hex string: ' + HexString);
    end;
  end; // for i
end;


function BytesToHexString(const bytes : TBytes): string;
var
  i : integer;
begin
  result := '';
  for i := low(bytes) to high(bytes) do
    result := result + IntToHex(bytes[i], 2) + ' ';

  result := trim(result);
end;

// Converts bytes to ascii chars. i.e. '41 42 43 20 20 20' to ascii 'ABC   '
// Non printable bytes returned as numbers in brackets. i.e. (10)
function BytesToAscii( bytes : TBytes ) : string;
var
  i : integer;
begin
  result := '';

  for i := 0 to length(bytes)-1 do begin
    case bytes[i] of
      32..126 : result := result + chr(bytes[i]);
      else result := result + '(' + inttostr(bytes[i]) + ')';
    end;
  end;

  // old: result := StringOf(bytes); // this returns unprintable chars..
end;


function AsciiStrToHexStr(const astr : string) : string;
begin
  result := BytesToHexString( BytesOf(astr) );
end;


function StrToShortMidiMsg( const aString : string ) : TShortMidiMsgRecord;
var
  s : AnsiString;
begin
  s := StringReplace( AnsiUpperCase( aString), ' ', '', [rfReplaceAll]);
  s := AddCharR('0', s, 6); // Pad with 0's to make 3 bytes

  if (length(s) > 6) then
    raise Exception.Create( 'Midi short message string must have a max of 6 chars (3 bytes): ' + aString);

  try
    result.aStatus := Byte(StrToInt('$' + copy(s, 1, 2)));
    result.aData1  := Byte(StrToInt('$' + copy(s, 3, 2)));
    result.aData2  := Byte(StrToInt('$' + copy(s, 5, 2)));
  except
    on EConvertError do
      raise Exception.Create('Invalid hex string: ' + astring);
  end;
end;


function RolandChecksum(const Data: array of Byte): Byte;
var
  i, Sum: Integer;
begin
  Sum := 0;
  for i := Low(Data) to High(Data) do
    Sum := Sum + Data[i];

  Result := Byte(128 - (Sum mod 128)); // 128 = 1000 0000
end;


// hstr format can be either: 'F0112233', 'F0 11 22 33'.
//   The string can contain brackets '[' or ']', which are deleted
//   The checksum will be placed after the closing bracket.
//   If there are no brackets, hstring is returned as is.
function ParseRolandChecksum(const hstr : string): string;
var
  startIndex, endIndex: Integer;
  s : string;
  csum : string;
begin
  result := hstr;
  startIndex := Pos('[', hstr);  // Find the opening bracket
  endIndex := Pos(']', hstr);    // Find the closing bracket

  if (startIndex > 0) and (endIndex > 0) then begin
    if (endIndex < startIndex) then
      raise Exception.create('ParseRolandChecksum: invalid input -> ' + hstr);

    s := Copy(hstr, startIndex + 1, endIndex - startIndex - 1);
    csum := ' ' + IntToHex( RolandChecksum(HexStringToBytes(s)), 2 ) + ' ';
    insert( csum, result, endindex );

    // strip the brackets
    result := StringReplace( result, '[', '', [rfReplaceAll] );
    result := StringReplace( result, ']', '', [rfReplaceAll] );
  end;
end;


function ConvertNumber(const numStr: string): string;
var
  num: qword;
  cleanStr: string;
  suffix: Char;
begin
  // Determine the suffix and base
  if Length(numStr) = 0 then
    raise Exception.Create('Input string is empty.');

  cleanstr := numstr;
  suffix := lowercase(numStr[Length(numStr)]);
  if suffix in ['d', 'b', 'h'] then
    setlength(cleanstr, Length(numStr)-1)
  else
    suffix := 'd';

  try
    case suffix of
      'h' : num := StrToQWord('$' + cleanStr);
      'b' : num := StrToQWord('%' + cleanStr);
    else
      num := StrToQWord(cleanStr);  // Assume decimal
    end;
  except
    on E: EConvertError do
      raise Exception.Create( E.Message + ^M^J'Invalid number format. Please provide a valid binary, decimal, or hexadecimal number.');
  end;

  // Convert the number to binary, decimal, and hexadecimal formats
  Result :=
       '  Decimal: ' + IntToStr(num) +
   ^M^J'  Hex:     ' + IntToHex(num, 0) +
   ^M^J'  Binary:  ' + IntToBin(num, 32, 8);
end;

end.

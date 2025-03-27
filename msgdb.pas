unit msgdb;

{$mode ObjFPC}{$H+}


{ --------------------------------------------------------------------------- }
interface
{ --------------------------------------------------------------------------- }


(*
This unit uses TStrings Name/Value pairs to add the functionality to
save / recall midi messages to a makeshift database.

It allows any printable character for the message name, and
allows line feeds in the values.

Loads the list from disk into memory at startup.
Saves the list to disk at shutdown (if it changed).
*)

uses
  Classes, SysUtils;

procedure LoadMsgKeys( asl : TStrings );
Procedure SaveMsg( const akey, amsg : string );
function ReadMsg( const akey : string) : string;
Procedure DeleteMsg( const akey : string );

{ --------------------------------------------------------------------------- }
implementation
{ --------------------------------------------------------------------------- }

var
  g_MsgList : TStringList;
  g_IsChanged : boolean;

const
  _VT_ = #11;  // CRLF chars are stored as #11 (vertical tab char)
  NAME_VALUE_SEPARATOR = #0; // For use in the string list. (allows '=' in the key name)
  MSG_DB_EXT = '.msgdb';


function GetMsgDbFn : string;
begin
  result := ExpandFileName(ChangeFileExt( ExtractFileName(paramstr(0) ), MSG_DB_EXT));
end;


procedure LoadMsgKeys(asl: TStrings);
var
  i : integer;
begin
  asl.clear;
  for i := 0 to g_MsgList.Count-1 do
    asl.Add(g_MsgList.Names[i]);
end;


procedure SaveMsg(const akey, amsg: string);
begin
  g_MsgList.Values[akey] := StringReplace( amsg, ^M^J, _VT_, [rfReplaceAll]);
  g_IsChanged := true;
end;


function ReadMsg(const akey : string): string;
begin
  result := StringReplace( g_MsgList.Values[akey], _VT_, ^M^J, [rfReplaceAll]);
end;


procedure DeleteMsg(const akey: string);
var
  ix : integer;
begin
  ix := g_MsgList.IndexOfName(akey);
  if ix >= 0 then begin
    g_MsgList.Delete(ix);
    g_IsChanged := true;
  end;
end;


procedure CreateDefaultMsgDb;
var
  b : byte;
  s : string;
begin
  SaveMsg( 'System Reset', 'FF' );
  SaveMsg( 'Device Identity Request', 'F0 7E 7F 06 01 F7' );
  SaveMsg( 'Program Change - 0', 'C0 00 00' );

  s := '';
  for b := $B0 to $BF do
    s := s + IntToHex( b, 2 ) + ' 78 00'^M^J;
  SaveMsg( 'All Sounds Off', trim(s) );

  s := StringReplace( s, ' 78 00', ' 79 00', [rfReplaceAll] );
  SaveMsg( 'Reset All Controllers', trim(s) );
end;


{ --------------------------------------------------------------------------- }
initialization
  g_MsgList := TStringList.Create;
  g_MsgList.NameValueSeparator := NAME_VALUE_SEPARATOR; // #0
  g_IsChanged := false;

  if FileExists(GetMsgDbFn) then
    g_MsgList.LoadFromFile(GetMsgDbFn)
  else
    CreateDefaultMsgDb; // Add a few default items.


{ --------------------------------------------------------------------------- }
finalization
  if g_IsChanged then begin
    g_MsgList.SaveToFile(GetMsgDbFn);
  end;



end.


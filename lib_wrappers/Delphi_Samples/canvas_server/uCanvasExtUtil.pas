unit uCanvasExtUtil;

interface

uses Classes;

procedure DeSerializeString(MS: TStream;var AString: string);
procedure SerializeString(MS: TStream;const AString: string);

implementation

procedure SerializeString(MS: TStream;const AString: string);
var
  Len: Cardinal;
begin
  Len := Length(AString);
  MS.Write(len, SizeOf(Len));

  if (Len > 0) then
    MS.WriteBuffer(AString[1], Len);
end;

procedure DeSerializeString(MS: TStream;var AString: string);
var
  Len: Cardinal;
  x: PChar;
begin
  MS.Read(Len, SizeOf(Len));
  if Len <= 0 then
    Exit;
  GetMem(x, Len+1);
  x[Len] := #0;
  MS.Read(x^, Len);
  AString := string(x);
  FreeMem(x);
end;

end.

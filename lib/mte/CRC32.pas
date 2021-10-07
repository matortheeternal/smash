unit CRC32;

interface

type
  Long = record
    LoWord: Word;
    HiWord: Word;
  end;

  // exported functions
function FileCRC32(FileName: string): string;
function StrCRC32(input: string): string;

const
  CRCPOLY = $EDB88320;

var
  CRCTable: array [0 .. 512] Of Longint;

implementation

{$WARNINGS OFF}

uses
  SysUtils;

procedure BuildCRCTable;
var
  i, j: Word;
  r: Longint;
begin
  FillChar(CRCTable, SizeOf(CRCTable), 0);
  for i := 0 to 255 do
  begin
    r := i shl 1;
    for j := 8 downto 0 do
      if (r and 1) <> 0 then
        r := (r Shr 1) xor CRCPOLY
      else
        r := r shr 1;
    CRCTable[i] := r;
  end;
end;

function RecountCRC(b: byte; CrcOld: Longint): Longint;
begin
  RecountCRC := CRCTable[byte(CrcOld xor Longint(b))
    ] xor ((CrcOld shr 8) and $00FFFFFF)
end;

function HextW(w: Word): string;
const
  h: array [0 .. 15] Of char = '0123456789ABCDEF';
begin
  HextW := '';
  HextW := h[Hi(w) shr 4] + h[Hi(w) and $F] + h[Lo(w) shr 4] + h[Lo(w) and $F];
end;

function HextL(l: Longint): string;
begin
  with Long(l) do
    HextL := HextW(HiWord) + HextW(LoWord);
end;

function FileCRC32(FileName: string): string;
var
  Buffer: PChar;
  F: File of byte;
  b: array [0 .. 255] of byte;
  CRC: Longint;
  e, i: Integer;
begin
  BuildCRCTable;
  CRC := $FFFFFFFF;
  AssignFile(F, FileName);
  FileMode := 0;
  Reset(F);
  GetMem(Buffer, SizeOf(b));
  repeat
    FillChar(b, SizeOf(b), 0);
    BlockRead(F, b, SizeOf(b), e);
    for i := 0 to (e - 1) do
      CRC := RecountCRC(b[i], CRC);
  until (e < 255) or (IOresult <> 0);
  FreeMem(Buffer, SizeOf(b));
  CloseFile(F);
  CRC := Not CRC;
  Result := HextL(CRC);
end;

function StrCRC32(input: string): string;
var
  b: TArray<byte>;
  CRC: Longint;
  i: Integer;
begin
  BuildCRCTable;
  CRC := $FFFFFFFF;
  b := TEncoding.UTF8.GetBytes(input);
  for i := 0 to Pred(Length(b)) do
    CRC := RecountCRC(b[i], CRC);
  CRC := Not CRC;
  Result := HextL(CRC);
end;

end.

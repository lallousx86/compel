unit CompelUtil;

interface

uses Windows;

function ComputeTempFileName(const aStrPrefix: string) : string;
function ComputerFileSize(const aFileName: string): Int64;

implementation


function ComputeTempFileName(const aStrPrefix: string) : string;
var
  tempPath, tempFile: array[0..MAX_PATH] of char;
begin
  Result := '';

  if Windows.GetTempPath(MAX_PATH, tempPath) = 0 then
    Exit;
  if Windows.GetTempFileName(tempPath, PChar(aStrPrefix), 0, tempFile) = 0 then
    Exit;
  Result := tempFile;
end;

function ComputerFileSize(const aFileName: string): Int64;
var
  fd: TWin32FindData;
  hFind: THandle;
begin
  hFind := FindFirstFile(PChar(aFileName), fd);
  if hFind = Windows.INVALID_HANDLE_VALUE then
  begin
    Result := -1;
    Exit;
  end;
  Result := (fd.nFileSizeHigh shl 32) or fd.nFileSizeLow;
  FindClose(hFind);
end;

end.

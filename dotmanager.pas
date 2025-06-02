program dotmanager;
{$mode objfpc}{$H+}{$C+}

uses SysUtils, fpjson, jsonparser, Process;

type
  TDotfile = record
    Name: string;
    Location: string;
    IsDirty: Boolean;
    MultiFiles: Boolean;
  end;
{
function SyncLocalRepo(ALocalRepo: String; ADotfiles: array of TDotfile): Boolean;
begin

end;
}
function SyncRemoteRepo(ARemoteRepo: String; ADotfiles: array of TDotfile): Boolean;
begin
  {NOTE: this is just for pushing the local to remote repo}
  {NOTE: First check if the config folder isDiry then maybe copy to the temp local repo }
  {NOTE: then push the temp local repo to the remote repo}
  {NOTE: After doing that fetch the remote repo and restor the files to the config folder}
end;

procedure RestoreDotFiles(ADotfiles: array of TDotfile);
begin
    {NOTE: This will be useful when using it in a new system}
    {NOTE: For now it doesn't matter yet}
end;



function GetDirtyFiles(Adotfiles: array of TDotfile): TArray<TDotfile>;
var
  I, J: Integer;
  Output: string;
  Lines: TStringList;
  Dirty: array of TDotfile;
  DotfileName: string;
begin
  Lines := TStringList.Create;
  try
    for I := 0 to High(Adotfiles) do
    begin
      if RunCommand('git', ['-C', Adotfiles[I].Location, 'status', '--porcelain'], Output) then
      begin
        Lines.Text := Output;
        for J := 0 to Lines.Count - 1 do
        begin
          if Trim(Lines[J]) = '' then
            Continue;
          DotfileName := Trim(Copy(Lines[J], 4, Length(Lines[J])));
          if SameText(DotfileName, Adotfiles[I].Name) then
          begin
            SetLength(Dirty, Length(Dirty) + 1);
            Dirty[High(Dirty)] := Adotfiles[I];
            Break; // no need to check other lines for this file
          end;
        end;
      end;
    end;

    Result := Dirty;
  finally
    Lines.Free;
  end;
end;

procedure CopyToLocalRepo(ALocalRepo: String; ADotfiles: array of TDotfile);
var
  I: Integer;
begin
{NOTE: this is useful incase I want a temp unified folder}
  for I := 0 to Length(ADotfiles) do
    begin
      if not Adotfiles[I].multifiles then
        begin
          if FileExists(Adotfiles[I].location + ADotfiles[I].Name) then
           begin
             WriteLn('Copying ' + Adotfiles[I].Name + ' to ' + ALocalRepo);
             CopyFile(Adotfiles[I].location + ADotfiles[I].Name, ALocalRepo + ADotfiles[I].Name);
           end else WriteLn('File not found: ' + Adotfiles[I].Name);
        end else 
        begin
          {TODO: copy all files and folders }
          Writeln('Multi Files DotFiles Not Implemented yet');
        end;
    end;
end;

function ReadDataFile(const AFileName: string): String;
var
  F: TextFile;
  Line: String;
begin
  Result := '';
  AssignFile(F, AFileName);
  Try
    Reset(F);
    while not Eof(F) do
      begin
        ReadLn(F, Line);
        Result := Result + Line;
      end;
  finally
    CloseFile(F);
  end;
end;

function GetDotfiles(): array of dotfile;
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  I: Integer;
  DotfilesList: array of TDotfile;
  Dotfile: TDotfile;
  FileContent: String;
begin
  {TODO: read dotfiles from a JSON}
  FileContent := ReadDataFile('dotfiles.json');
  JSONData := GetJSON(FileContent);
  try
  if JsonData.JSONType <> jtArray then
    begin
      JSONArray := TJSONArray(JSONData);
      SetLength(DotfilesList, JSONArray.Count);
      for I:= 0 to JSONArray.Count-1 do
        begin
          with TJSONObject(JSONArray.Items[I]) do
            begin
              Dotfile.Name := GetValue('Name');
              Dotfile.Location := GetValue('location');
              Dotfile.IsDirty := False;
              Dotfile.MultiFiles := GetValue('multifiles');
            end;
          DotfilesList[I] := Dotfile;
        end;
    end;
  Result := DotfilesList;
  Finally
    JsonData.Free;
  end;
end;

  {TODO: Copy those files to local repo}
  {TODO: Sync local repo with remote repo}

{TODO: read dotfiles from a JSON}

begin
end.

program dotmanager;
{$mode objfpc}{$H+}{$C+}

uses SysUtils, fpjson, jsonparser, Process, Classes;





type
  TDotfile = record
    Name: string;
    Location: string;
    IsDirty: Boolean;
    MultiFiles: Boolean;
  end;

  TDotFileArray = array of TDotFile;
  

function GetDirtyFiles(ADotfiles: array of TDotfile): TDotFileArray;
var
  I, J: Integer;
  Output: string;
  Lines: TStringList;
  Dirty: array of TDotfile;
  DotfileName: string;
begin
   SetLength(Dirty, 0);
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

procedure CopyFile(const Src, Dist: String);
var
   SourceStream, DistStream: TFileStream;
begin
   SourceStream:= TFileStream.Create(Src, fmOpenRead) ;
   try
      DistStream:= TFileStream.Create(Dist, fmCreate);
      try
         DistStream.copyFrom(SourceStream, SourceStream.Size);
      finally
        DistStream.Free
      end;
   finally
      SourceStream.Free;
   end;
end;



procedure CopyToLocalRepo(ALocalRepo: String; ADotfiles: array of TDotfile);
var
  I: Integer;
begin
{NOTE: this is useful incase I want a temp unified folder}
  for I := 0 to High(ADotfiles) do
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


procedure SyncLocalRepo(ALocalRepo: String; ADotfiles: array of TDotfile);
var 
  DirtyFiles: TDotFileArray;
  Output: String;
begin
  DirtyFiles := GetDirtyFiles(ADotfiles);
  if Length(DirtyFiles) = 0 then
    begin
      WriteLn('No dotfiles changes detected');
      Exit;
    end;
  CopyToLocalRepo(ALocalRepo, DirtyFiles);
  RunCommand('git', ['-C', ALocalRepo, 'add', '.'], Output);
  RunCommand('git', ['-C', ALocalRepo, 'commit', '-m', '"dotmanager sync"'], Output); {TODO: get an interactive prompt for commit message if chosen to by config}
  RunCommand('git', ['-C', ALocalRepo, 'push'], Output);
end;



function SyncRemoteRepo(ARemoteRepo: String; ADotfiles: array of TDotfile): Boolean;
begin
  {NOTE: this is just for pushing the local to remote repo}
  {NOTE: First check if the config folder isDiry then maybe copy to the temp local repo }
  {NOTE: then push the temp local repo to the remote repo}
  {NOTE: After doing that fetch the remote repo and restor the files to the config folder}
   Result:= True;
end;

procedure RestoreDotFiles(ADotfiles: array of TDotfile);
begin
    {NOTE: This will be useful when using it in a new system}
    {NOTE: For now it doesn't matter yet}
   
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

function GetDotfiles(): TDotFileArray;
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
  if JsonData.JSONType = jtArray then
    begin
      JSONArray := TJSONArray(JSONData);
      SetLength(DotfilesList, JSONArray.Count);
      for I:= 0 to JSONArray.Count-1 do
        begin
          with TJSONObject(JSONArray.Items[I]) do
            begin
              Dotfile.Name := Get('name', '');
              Dotfile.Location := Get('location', '');
              Dotfile.IsDirty := False;
              Dotfile.MultiFiles := Get('multifiles', False);
            end;
          DotfilesList[I] := Dotfile;
        end;
    end;
  Result := DotfilesList;
  Finally
    JsonData.Free;
  end;
end;

procedure ListDotfiles(ADotfiles: array of TDotfile);
var
  DirtyFiles: TDotFileArray;
  I: Integer;
begin
  DirtyFiles := GetDirtyFiles(ADotfiles);
  for I := 0 to High(DirtyFiles) do
    begin
      Write(DirtyFiles[I].Name + ' : ');
      if Adotfiles[I].IsDirty then
        WriteLn('Changed') {TODO: make it red or something}
      else
        WriteLn('Not Changed');{TODO: Green color}
    end;
end;





begin
end.

program dotmanager;
{$mode objfpc}{$H+}{$C+}

uses SysUtils, fpjson, jsonparser, Process, Classes, Crt, IniFiles;



type
  TDotfile = record
    Name: string;
    Location: string;
    IsDirty: Boolean;
    MultiFiles: Boolean;
  end;
  TConfig = record
    LocalRepo: String;
    RemoteRepo: String;
  end;

  TDotFileArray = array of TDotFile;
  

function GetDirtyFiles(ADotfiles: array of TDotfile): TDotFileArray;
var
  I, J: Integer;
  Output: string;
  Lines: TStringList;
  Dirty: array of TDotfile;
  DotfileName: string;
  Config: TConfig;
begin
   SetLength(Dirty, 0);
   config := ReadConfigFile('./config.ini');
  Lines := TStringList.Create;
  try
    for I := 0 to High(Adotfiles) do
    begin
      if RunCommand('git', ['-C', config.localRepo, 'status', '--porcelain'], Output) = 0 then
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


procudere AutoSync(source, localRepo: String);
var
  
begin
   
end;


procedure SyncLocalRepo(ALocalRepo: String; ADotfiles: array of TDotfile);
var 
  DirtyFiles: TDotFileArray;
  Output: String;
  I: Integer;
begin
  DirtyFiles := GetDirtyFiles(ADotfiles);
  if Length(DirtyFiles) = 0 then
    begin
      WriteLn('No dotfiles changes detected');
      Exit;
    end;
  CopyToLocalRepo(ALocalRepo, DirtyFiles);
  {TODO: check if is a git repo using .git dir, if not just git init}
  RunCommand('git', ['-C', ALocalRepo, 'add', '.'], Output);
  RunCommand('git', ['-C', ALocalRepo, 'commit', '-m', '"dotmanager sync"'], Output); {TODO: get an interactive prompt for commit message if chosen to by config}
  {RunCommand('git', ['-C', ALocalRepo, 'push'], Output);}

  {Cleaning}
  for i := 0 to High(DirtyFiles) do
    dirtyFiles[i].IsDirty := False;

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
  SetLength(DotfilesList, 0);
  try
  try
  FileContent := ReadDataFile('./dotfiles.json');
  JSONData := GetJSON(FileContent);
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
  Except
    on E: Exception do
      begin
        WriteLn('Error reading dotfiles.json ', E.Message);
      end;
    end;
  Finally
    JsonData.Free;
  end;
  Result := DotfilesList;
end;

procedure ListDotfiles(ADotfiles: array of TDotfile);
var
  I: Integer;
begin
  WriteLn('Listing dotfiles: ', length(ADotfiles));
  for I := 0 to High(Adotfiles) do
    begin
      if Adotfiles[I].IsDirty then
        begin
          TextColor(Red);
          Writeln(Adotfiles[I].Name + ' : ');
        end
      else 
        begin
          TextColor(Green);
          Writeln(Adotfiles[I].Name + ' : ');
        end;
    end;
end;

function ReadConfigFile(const AFileName: string): TConfig;
var
  ConfigIni: TIniFile;
  config: TConfig;
begin
  ConfigIni := TIniFile.Create(AFileName);
  try
    config.LocalRepo := ConfigIni.ReadString('Config', 'local_repository', '../local/');
    config.RemoteRepo := ConfigIni.ReadString('Config', 'remote_repository', '');
    Result := config;
  finally
    ConfigIni.Free;
  end;
end;


var
  dotfiles: TDotFileArray;
  dirtyFiles: TdotFileArray;
  config: TConfig;
begin
  {TODO: read commands from command line arguments}
  {setLength(dotfiles, 0);}
  dotfiles := GetDotfiles();
  listDotfiles(dotfiles);
  config := ReadConfigFile('./config.ini');
  {TODO: if first use, restore files if they exists in remote repo by reading the config}
  {TODO: or wait for restore command}
  {TODO: if not then check for dirty files then sync them}

  {dirtyFiles := GetDirtyFiles(dotfiles);}
  {copyToLocalRepo(localRepo, dirtyFiles);}
  {syncLocalRepo(localRepo, dirtyFiles);}
end.

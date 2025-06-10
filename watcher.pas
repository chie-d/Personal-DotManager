 unit watcher;

 {$mode fpc}{$H+}{$C+}

 interface

 uses 
  classes, SysUtils, FileUtil;

  type
    TSyncCallback - procedure(const changedFile: String);

    procedure StartWatching(const SourceDir: String; const Callback: TSyncCallback);
    procedure StopWatching;

implementation

var 
  Watcher: TFileSystemWatcher;
  syncCallback: TSyncCallback;

procedure OnChanged(const Sender: TObject; change: TWatchChange; const FileName: string);
begin
  if Assigned(syncCallback) then
    SyncCallback(FileName);
end;

procedure StartWatching(const SourceDir: String; const Callback: TSyncCallback);
begin
  if Assigned(Watcher) then
    Exit;
  SyncCallback := Callback;
  Watcher := TFileSystemWatcher.Create;
  Watcher.Directory:= SourceDir;
  Watcher.OnChange := @OnChange;
  Watcher.Active := True;
end;

procedure StopWatching;
begin
  FreeAndNil(Watcher);
end;

end.


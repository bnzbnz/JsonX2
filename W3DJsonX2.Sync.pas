(*****************************************************************************
MIT License

Copyright (c) 2017 Dmitriy Sorokin

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*****************************************************************************)

unit W3DJsonX2.Sync;

interface

uses
  System.Generics.Collections;

type
  TThreadDict<TKey, TValue> = class
  private
    FDict: TDictionary<TKey, TValue>;
    FLock: TObject;

    procedure Lock;
    procedure Unlock;
  public
    procedure Add(const AKey: TKey; const AValue: TValue);
    procedure Remove(const AKey: TKey);
    procedure Clear;
    procedure SetValue(const AKey: TKey; const AValue: TValue);

    // vvv Non-blocking methods vvv
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean;
    function Count: Integer;
    // ^^^ Non-blocking methods ^^^

    function ItemsCopy: TDictionary<TKey, TValue>;

    // It is not threadsave if you use pointer after Unlocking
    function LockPointer: TDictionary<TKey, TValue>;
    procedure UnlockPointer;

    constructor Create;
    destructor Destroy; override;
  end;

  TThreadList<TValue> = class
  private
    FList: TList<TValue>;
    FLock: TObject;
    procedure Lock;
    procedure Unlock;
  public
    procedure Add(const AValue: TValue);
    procedure Remove(const AValue: TValue);
    procedure Delete(const AIndex: Integer);
    procedure Clear;
    procedure Swap(const AIndex1, AIndex2: Integer); overload;
    procedure Move(const ACurIndex, ANewIndex: Integer);
    procedure SetValue(const AIndex: Integer; AValue: TValue);

    // vvv Non-blocking methods vvv
    function TryGetValue(const AIndex: Integer; out AValue: TValue): Boolean;
    function Contains(const AItem: TValue): Boolean;
    function IndexOf(const AItem: TValue): Integer;
    function Count: Integer;
    // ^^^ Non-blocking methods ^^^

    function ItemsCopy: TList<TValue>;

    // It is not threadsave if you use pointer after Unlocking
    function LockPointer: TList<TValue>;
    procedure UnlockPointer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadDict<TKey, TValue> }

procedure TThreadDict<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  Lock;
  try
    FDict.Add(AKey, AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.Clear;
begin
  Lock;
  try
    FDict.Clear;
  finally
    Unlock;
  end;
end;

function TThreadDict<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := FDict.ContainsKey(AKey);
end;

function TThreadDict<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
begin
  Result := FDict.ContainsValue(AValue);
end;

function TThreadDict<TKey, TValue>.Count: Integer;
begin
  Result := FDict.Count;
end;

constructor TThreadDict<TKey, TValue>.Create;
begin
  FLock := TObject.Create;
  FDict := TDictionary<TKey, TValue>.Create;
end;

destructor TThreadDict<TKey, TValue>.Destroy;
begin
  Lock;
  try
    FDict.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
end;

function TThreadDict<TKey, TValue>.ItemsCopy: TDictionary<TKey, TValue>;
begin
  Lock;
  try
    Result := TDictionary<TKey, TValue>.Create(FDict);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadDict<TKey, TValue>.LockPointer: TDictionary<TKey, TValue>;
begin
  Lock;
  Result := FDict;
end;

procedure TThreadDict<TKey, TValue>.Remove(const AKey: TKey);
begin
  Lock;
  try
    FDict.Remove(AKey);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.SetValue(const AKey: TKey;
  const AValue: TValue);
begin
  Lock;
  try
    FDict[AKey] := AValue;
  finally
    Unlock;
  end;
end;

function TThreadDict<TKey, TValue>.TryGetValue(const AKey: TKey;
  out AValue: TValue): Boolean;
begin
  try
    AValue := FDict[AKey];
    Exit(True);
  except
    Exit(False);
  end;
end;

procedure TThreadDict<TKey, TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

procedure TThreadDict<TKey, TValue>.UnlockPointer;
begin
  Unlock;
end;

{ TThreadList<TValue> }

procedure TThreadList<TValue>.Add(const AValue: TValue);
begin
  Lock;
  try
    FList.Add(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadList<TValue>.Clear;
begin
  Lock;
  try
    FList.Clear;
  finally
    Unlock;
  end;
end;

function TThreadList<TValue>.Contains(const AItem: TValue): Boolean;
begin
  Result := FList.Contains(AItem);
end;

function TThreadList<TValue>.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TThreadList<TValue>.Create;
begin
  FLock := TObject.Create;
  FList := TList<TValue>.Create;
end;

procedure TThreadList<TValue>.Delete(const AIndex: Integer);
begin
  Lock;
  try
    FList.Delete(AIndex);
  finally
    Unlock;
  end;
end;

destructor TThreadList<TValue>.Destroy;
begin
  Lock;
  try
    FList.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
end;

function TThreadList<TValue>.IndexOf(const AItem: TValue): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

function TThreadList<TValue>.ItemsCopy: TList<TValue>;
begin
  Lock;
  try
    Result := TList<TValue>.Create(FList);
  finally
    Unlock;
  end;

end;

procedure TThreadList<TValue>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadList<TValue>.LockPointer: TList<TValue>;
begin
  Lock;
  Result := FList;
end;

procedure TThreadList<TValue>.Move(const ACurIndex, ANewIndex: Integer);
begin
  Lock;
  try
    FList.Move(ACurIndex, ANewIndex);
  finally
    Unlock;
  end;
end;

procedure TThreadList<TValue>.Remove(const AValue: TValue);
begin
  Lock;
  try
    FList.Remove(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadList<TValue>.SetValue(const AIndex: Integer; AValue: TValue);
begin
  Lock;
  try
    FList[AIndex] := AValue;
  finally
    Unlock;
  end;
end;

procedure TThreadList<TValue>.Swap(const AIndex1, AIndex2: Integer);
var
  vTmpItem: TValue;
begin
  Lock;
  try
    vTmpItem := FList[AIndex1];
    FList[AIndex1] := FList[AIndex2];
    FList[AIndex2] := vTmpItem;
  finally
    Unlock;
  end;
end;

function TThreadList<TValue>.TryGetValue(const AIndex: Integer; out AValue: TValue): Boolean;
begin
  try
    AValue := FList[AIndex];
    Exit(True);
  except
    Exit(False);
  end;
end;

procedure TThreadList<TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

procedure TThreadList<TValue>.UnlockPointer;
begin
  Unlock;
end;

end.

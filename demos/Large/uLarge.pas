unit uLarge;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts
  , System.Generics.Collections
  , RTTI
  , W3DJsonX2.Obj
  , W3DJsonX2
  , W3DJsonX2.RTTI
  , W3DJsonX2.Types
  , W3DJsonX2.Conv
  , W3DJsonX2.Utils
  , W3DCloneable, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, FMX.StdCtrls
  ;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IJX2ObjListX = Interface
    ['{0B303C81-3234-4030-9C72-E2DB0AE75445}']
  end;

   TIList<T> = class(TList<T>, IJX2ObjListX)
   protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;


  FvalueConst = class(TJX2)
    applicableForLocalizedAspectName: variant;
    applicableForLocalizedAspectValues: TJX2ValueList;
  end;

  FvalueConstraint = class(TJX2)
    localizedValue: TValue;
    [JX2AttrClass(FvalueConst)]
    valueConstraints : TJX2ObjList;
    applicableForLocalizedAspectName: TValue;
    [JX2AttrConv(TIJX2ValueListConv)]
    applicableForLocalizedAspectValues: TList<TValue>;
  end;

  TFaspectValues = class(TJX2)
    localizedValue: TValue;
    [JX2AttrConv(TIJX2ObjectListConv, FvalueConstraint)]
    valueConstraints: TObjectList<TObject>;
  end;

  TaspectConstraint = class(TJX2)
    aspectDataType: TValue;
    itemToAspectCardinality: TValue;
    aspectMode: TValue;
    aspectRequired: TValue;
    aspectUsage: TValue;
    aspectEnabledForVariations: TValue;
    aspectApplicableTo: TJX2ValueList;
    aspectMaxLength: TValue;
    expectedRequiredByDate: TValue;
    aspectFormat: TValue;
  end;

  TcategoryAspectName = class(TJX2)
    categoryId: TValue;
    categoryName: TValue;
  end;

  TcategoryAspect = class(TJX2)
    localizedAspectName: TValue;
    aspectConstraint: TaspectConstraint;
    [JX2AttrConv(TIJX2ObjectListConv, TFaspectValues)]
    aspectValues: TObjectList<TObject>;
  end;

  TcategoryAspects = class(TJX2)
    category: TcategoryAspectName;
    [JX2AttrClass(TcategoryAspect)]
    aspects: TJX2ObjList; // or TIJX2ObjectListConv;
  end;

  TfetchItemAspectsContentType = class(TJX2)
  public
    categoryTreeId: TValue;
    categoryTreeVersion: TValue;
    [JX2AttrConv(TIJX2ObjectListConv, TcategoryAspects)]
    categoryAspects: TObjectList<TObject>;
  end;

var
  Form2: TForm2;

implementation
uses System.Diagnostics;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
  Sw: TStopWatch;
  Obj: TfetchItemAspectsContentType;
begin
  Obj := nil;
  try
    var A : TJX2DataBlock;

    OpenDialog1.InitialDir := ExtractFilePath( ParamStr(0) );
    if not OpenDialog1.Execute then Exit;
    Sw := TStopWatch.StartNew;
    Memo1.Lines.Add('Filename : ' + OpenDialog1.Filename);
    var Json := LoadStringFromFile(OpenDialog1.Filename, TEncoding.ANSI);
    SW.Stop;
    Memo1.Lines.Add('Read in : ' + Sw.ElapsedMilliseconds.ToString + 'ms');
    Sw.Start;
    Obj := W3DJX2.Deserialize<TfetchItemAspectsContentType>(Json, []);
    Sw.Stop;
    Memo1.Lines.Add('ToJson in : ' + Sw.ElapsedMilliseconds.ToString + 'ms');
    Sw.Start;
    var AspectValueCount: Integer;
    if assigned(Obj.categoryAspects) then for var i in Obj.categoryAspects do
      if assigned(TcategoryAspects(i).aspects) then for var t in TcategoryAspects(i).aspects do
        if assigned(TcategoryAspect(t).aspectValues) then for var j in TcategoryAspect(t).aspectValues do
          Inc(AspectValueCount);
    Memo1.Lines.Add('eBay Aspect values : ' + AspectValueCount.ToString + ' (TObject Created)');
    Sw.Stop;
    Memo1.Lines.Add('Total Time : ' + Sw.ElapsedMilliseconds.ToString + 'ms');

  finally
    Obj.Free;
  end;
end;


{ TIJX2ObjListX<T> }

function TIList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TIList<T>._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TIList<T>._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

end.

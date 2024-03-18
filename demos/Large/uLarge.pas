unit uLarge;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  System.Generics.Collections, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, FMX.StdCtrls
  , RTTI
  , W3DJsonX2.Obj
  , W3DJsonX2
  , W3DJsonX2.RTTI
  , W3DJsonX2.Types
  , W3DJsonX2.Conv
  , W3DJsonX2.Utils
  , W3DCloneable
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

  FvalueConst = class(TJX2)
    applicableForLocalizedAspectName: variant;
    applicableForLocalizedAspectValues: TJX2ValueList;
  end;

  FvalueConstraint = class(TJX2)
    localizedValue: TValue;
    [JX2AttrClass(FvalueConst)]
    valueConstraints : TJX2ObjList;
    applicableForLocalizedAspectName: TValue;
    applicableForLocalizedAspectValues: TJX2ValueList
  end;

  TFaspectValues = class(TJX2)
    localizedValue: TValue;
    [JX2AttrClass(FvalueConstraint)]
    valueConstraints: TJX2ObjList;
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
    [JX2AttrClass(TFaspectValues)]
    aspectValues: TJX2ObjList;
  end;

  TcategoryAspects = class(TJX2)
    category: TcategoryAspectName;
    [JX2AttrClass(TcategoryAspect)]  // Using a predefined Object List
    aspects: TJX2ObjList;
  end;

  TfetchItemAspectsContentType = class(TJX2)
  public
    categoryTreeId: TValue;
    categoryTreeVersion: TValue;
    [JX2AttrClass(TcategoryAspects)]  // Using a predefined Object List
    categoryAspects: TJX2ObjList;
  end;

  TObjectConstructor = function : TObject;



var
  Form2: TForm2;

implementation
uses System.Diagnostics;

{$R *.fmx}
procedure TForm2.Button1Click(Sender: TObject);
var
  Sw: TStopWatch;
  Obj : TfetchItemAspectsContentType;
  Json: string;
begin
  Obj := nil;
  try
    OpenDialog1.InitialDir := ExtractFilePath( ParamStr(0) );
    if not OpenDialog1.Execute then Exit;
    Sw := TStopWatch.StartNew;
    Memo1.Lines.Add('Filename : ' + OpenDialog1.Filename);
    Json := LoadStringFromFile(OpenDialog1.Filename, TEncoding.UTF8);
    SW.Stop;
    Memo1.Lines.Add('Read in : ' + Sw.ElapsedMilliseconds.ToString + 'ms');
    SW.Start;
    Obj := W3DJX2.Deserialize<TfetchItemAspectsContentType>(Json);
    Sw.Stop;
    Memo1.Lines.Add('ToJson in : ' + Sw.ElapsedMilliseconds.ToString + 'ms');
    var AspectValueCount: Integer;
    if not Assigned(Obj) then Exit;
    if assigned(Obj.categoryAspects) then for var i in Obj.categoryAspects do
      if assigned(TcategoryAspects(i).aspects) then for var j in TcategoryAspects(i).aspects do
        if assigned(TcategoryAspect(j).aspectValues) then for var k in TcategoryAspect(j).aspectValues do
          Inc(AspectValueCount);
    Memo1.Lines.Add('eBay Aspect values count : ' + AspectValueCount.ToString + ' (TObject Created)');
    Sw.Reset; Sw.Start;
    SaveStringToFile('Json.json', W3DJX2.Serialize(Obj), TEncoding.UTF8 );
    Memo1.Lines.Add(' From Json to File :' + Sw.ElapsedMilliseconds.ToString + 'ms');
  finally
    Obj.Free;
  end;
end;

end.

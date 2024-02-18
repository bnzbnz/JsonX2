unit W3DCloneable;

interface

type

  IW3DCloneable = interface
  ['{C6437503-760C-47EA-8F61-4B7F20BE9CA9}']
    function Clone : IW3DCloneable;
  end;

  function Clone(AObj : IInterface): IW3DCloneable;


implementation

function Clone(AObj : IInterface): IW3DCloneable;
begin
  Result := (AObj as IW3DCloneable).Clone;
end;


end.


unit W3DCloneable;

interface

type

  IW3DCloneable = interface
  ['{222EC327-E2CC-4002-B8E0-0FEF946F04AC}']
    function Clone : IW3DCloneable;
  end;

  function Clone(AObj : IInterface): IW3DCloneable;


implementation

function Clone(AObj : IInterface): IW3DCloneable;
begin
  Result := (AObj as IW3DCloneable).Clone;
end;


end.


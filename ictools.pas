{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ictools;

{$warn 5023 off : no warning about unused units}
interface

uses
  ICSdfDataset, ICXMLConfig, ICGaugeIndicator, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ICSdfDataset', @ICSdfDataset.Register);
  RegisterUnit('ICXMLConfig', @ICXMLConfig.Register);
  RegisterUnit('ICGaugeIndicator', @ICGaugeIndicator.Register);
end;

initialization
  RegisterPackage('ictools', @Register);
end.
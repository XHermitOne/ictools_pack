{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ictools;

{$warn 5023 off : no warning about unused units}
interface

uses
  ICSdfDataset, ICXMLConfig, ICAnalogGauge, ICLogComboBoxEx, ICLogImageList, 
  edit_xml_config_form, ICRefObjDataSource, choice_ref_obj_form, 
  icrefobjchoiceeditbutton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ICSdfDataset', @ICSdfDataset.Register);
  RegisterUnit('ICXMLConfig', @ICXMLConfig.Register);
  RegisterUnit('ICAnalogGauge', @ICAnalogGauge.Register);
  RegisterUnit('ICLogComboBoxEx', @ICLogComboBoxEx.Register);
  RegisterUnit('ICLogImageList', @ICLogImageList.Register);
  RegisterUnit('ICRefObjDataSource', @ICRefObjDataSource.Register);
  RegisterUnit('icrefobjchoiceeditbutton', @icrefobjchoiceeditbutton.Register);
end;

initialization
  RegisterPackage('ictools', @Register);
end.

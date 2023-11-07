unit choice_ref_obj_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  TreeListView;

type

  { TChoiceRefObjForm }

  TChoiceRefObjForm = class(TForm)
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    EditBitBtn: TBitBtn;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    TreeListView1: TTreeListView;
  private

  public

  end;

var
  ChoiceRefObjForm: TChoiceRefObjForm;

implementation

{$R *.lfm}

end.


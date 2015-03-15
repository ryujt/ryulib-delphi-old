unit RegisterRyuOpenToolsAPI;

interface

implementation

uses
  CreateAbstractPresentationCore,
  CreateAbstractPresentationForm,
  CreateAbstractPresentationFrame,
  CreateAbstractPresentationView,
  CreateClassUnit,
  CreateComponentUnit,
  CreateSingletonUnit,
  Windows, SysUtils, Classes, ToolsAPI;


initialization
  (BorlandIDEServices as IOTAGalleryCategoryManager).AddCategory('Ryu', 'Ryu');

  RegisterPackageWizard(
    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
    TCreateAbstractPresentationCore.Create(
      'RYU',
      'CreateAbstractPresentationCore',
      'Presentation Core',
      'Ryu',
      'Create Abstract Presentation Core'
    )
  );

  RegisterPackageWizard(
    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
    TCreateAbstractPresentationForm.Create(
      'RYU',
      'CreateAbstractPresentationForm',
      'Presentation Form',
      'Ryu',
      'Create Abstract Presentation Form'
    )
  );

  RegisterPackageWizard(
    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
    TCreateAbstractPresentationFrame.Create(
      'RYU',
      'CreateAbstractPresentationFrame',
      'Presentation Frame',
      'Ryu',
      'Create Abstract Presentation Frame'
    )
  );

  RegisterPackageWizard(
    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
    TCreateAbstractPresentationView.Create(
      'RYU',
      'CreateAbstractPresentationView',
      'Presentation View',
      'Ryu',
      'Create Abstract Presentation View'
    )
  );

  RegisterPackageWizard(
    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
    TCreateClassUnitWizard.Create(
      'RYU',
      'CreateClassUnitWizard',
      'Class Unit',
      'Ryu',
      'Create Class Unit Wizard'
    )
  );

  RegisterPackageWizard(
    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
    TCreateComponentUnitWizard.Create(
      'RYU',
      'CreateComponentUnitWizard',
      'Component Unit',
      'Ryu',
      'Create Component Unit Wizard'
    )
  );

  RegisterPackageWizard(
    // AIcon, AIDString, AName, AAuthor, ACommnet, APage
    TCreateSingletonUnitWizard.Create(
      'RYU',
      'SingletonUnitCreator',
      'Singleton Unit',
      'Ryu',
      'Create Singleton Unit Wizard'
    )
  );
end.

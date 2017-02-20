# Description
Lara's Croft braid utility is intented to add a braid model into .phd (Tomb Raider 1 file)

The TR1 (DOS and later Windows versions) engine doesn't support rendering of a Lara's braid.
(Even though she appears with the braid in cutscenes)

The later open source indy projects (such as [OpenLara](https://github.com/XProger/OpenLara)) adds support for the it.
However, it's using level resources and depends on the model to be in .phd file.

The utility adds the model into a .phd file.


# How to Use

run the utility and pass .phd file file

`larasbraid level1.phd`

if it's identified that the file specified is TR1 file and it doesn't have a braid model.
The new file will be generated with the model added. The resultign file name would be changed to *-braided.phd


# Building

The utility requires [Free Pascal Compiler](http://freepascal.org) (pretty much any available version)

`fpc larasbraid.lpr`

With some smaller adjustements (i.e. adding $IFDEFs for ${mode delphi} directive), could be 
compiled by Delphi compiler as well.
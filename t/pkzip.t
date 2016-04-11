# Setup

```
- CM.make "pkzip.cm";
...
val it = true : bool
- fun lszip fileName =
=       let
=         val pkzip = Pkzip.openIn fileName
=         fun println s = print (s ^ "\n")
=       in
=         List.app (println o #fileName) (Pkzip.entries pkzip);
=         Pkzip.closeIn pkzip
=       end;
...
val lszip = fn : string -> unit
-  fun catzip zipFileName entryFileName =
=        let
=          val pkzip = Pkzip.openIn zipFileName
=          val SOME entry = Pkzip.findEntry (pkzip, entryFileName)
=          val data = Pkzip.readEntry (pkzip, entry)
=        in
=          print (Byte.bytesToString data);
=          Pkzip.closeIn pkzip
=        end;
...
val catzip = fn : string -> string -> unit
```

# lszip: excel.xslx

```
- lszip "testsuite/fixture/excel.xlsx";
[Content_Types].xml
_rels/.rels
xl/_rels/workbook.xml.rels
xl/workbook.xml
xl/theme/theme1.xml
xl/worksheets/sheet2.xml
xl/worksheets/_rels/sheet1.xml.rels
xl/worksheets/_rels/sheet2.xml.rels
xl/worksheets/_rels/sheet3.xml.rels
xl/worksheets/sheet3.xml
xl/worksheets/sheet1.xml
xl/styles.xml
docProps/core.xml
xl/printerSettings/printerSettings1.bin
xl/printerSettings/printerSettings2.bin
xl/printerSettings/printerSettings3.bin
docProps/app.xml
val it = () : unit
```

# lszip: Java archive
```
- lszip "testsuite/fixture/jar.jar";
META-INF/
META-INF/MANIFEST.MF
val it = () : unit
```

# lszip: Java archive (uncompressed)
```
- lszip "testsuite/fixture/jar0.jar";
META-INF/
META-INF/MANIFEST.MF
val it = () : unit
```

# catzip: excel.xslx
```
- catzip "testsuite/fixture/excel.xlsx" "xl/workbook.xml";
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><fileVersion appName="xl" lastEdited="5" lowestEdited="4" rupBuild="9303"/><workbookPr filterPrivacy="1" defaultThemeVersion="124226"/><bookViews><workbookView xWindow="240" yWindow="105" windowWidth="14805" windowHeight="8010"/></bookViews><sheets><sheet name="Sheet1" sheetId="1" r:id="rId1"/><sheet name="Sheet2" sheetId="2" r:id="rId2"/><sheet name="Sheet3" sheetId="3" r:id="rId3"/></sheets><calcPr calcId="122211"/></workbook>val it = () : unit
```

# catzip: Java archive
```
- catzip "testsuite/fixture/jar.jar" "META-INF/MANIFEST.MF";
Manifest-Version: 1.0
Created-By: 1.8.0_66 (Oracle Corporation)

val it = () : unit
```

# catzip: Java archive (uncompressed)
```
- catzip "testsuite/fixture/jar0.jar" "META-INF/MANIFEST.MF";
Manifest-Version: 1.0
Created-By: 1.8.0_66 (Oracle Corporation)

val it = () : unit
```

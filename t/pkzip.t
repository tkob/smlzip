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

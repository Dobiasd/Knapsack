guter generator: http://www.racoindustries.com/barcodegenerator/1d/upc-13.aspx
Guter Info-Seite zu Barcodes: http://www.barcodeisland.com/

todo: einzelne code-typen einzeln aktivieren koennen

todo: unit tests fuer jedes modul


todo:
Generate perfect Barcodes of different types.
take digit list
code to binary bar information
draw bars
imperfect ink and paper color
ink smear
paper wave or perspective transform (x wave)
(bend barcode ?)
masking
added irregular intensity (foil reflexion)
lens unsharpness (gauss)
pixelization ()
noise
gamma (dafuer bild einfach in photomenmengenbild umwandeln)
  http://entropymine.com/imageworsener/gamma/
  http://www.4p8.com/eric.brasseur/gamma.html#Assume_a_gamma_of_2.2
  def linear_to_gamma (s):
      return s ** (1/2.2)
  def gamma_to_linear (s):
      return s ** 2.2

todo:
Erkennung:
Barcode ausschneiden
Winkel feststellen
Eventuell um 90° drehen, sodass der horizontate Gradient überwiegt
Grob start, ende, modulbreite und digit positions finden
digits perfekt generieren (gleicher winkel, komplette bildhöhe)
subpixelig matchen (dafür digits jedes mal rendern), nur x shift
  dabei auch die bars, die immer am ende vom vor-digit und
  am Anfang vom folge-digit kommen, damit es besser einrastet.
optimieren über col_black, col_white, blur_sigma, ink_smear_factor usw.






ctor hiden?


repa benutzen, danach accelerate


Malfunktionen:
type Shape = (Int, Int) -> Double
data drawStyle = add | mult | overwrite
draw :: ImageSize -> drawStyle -> [Shape] -> Image

flowDown und so

Test der selbst testbild malt und großes bild erzeugt (namen dranschreibt?)

andere farbraeume. Kennzeichen immer mit am Image haben. oder einfach immer alle channels? sollte doch nix kosten weil lazy und so.

erode, dilate


interpolation beim warpen:
Richtige Variante wäre es, den Zielpixel als Viereck auf das Quellbild
zu warpen, und da dann die Duchschnittsfarbe zu nehmen von dem,
was er überdecke. Dafür muss man die Interpolation beim Quellbild dann
integrieren.
Wenn man nicht interpoliert (bzw nearest neighbour nimmt)
muss man einfach die schnittflächen des gebackwarpten pixels
mit den quellpixeln berechnen.
Dafuer kann man alles (quellquadrate und zielviereck) in jeweils
zwei dreiecke aufteilen, und dann da die schnittflaechen berechnen.
Ob der gebackwarpte pixel nicht-simpel (gefaltet) ist, sieht man daran,
dass der flächeninhalt seiner beiden dreiecke (kreuzprodukt)
einmal negativ ist. Dann muessen die beiden dreiecke anders berechnet
werden, naemlich ueber den schnittpunkt der knickung.






OR3:
- Bild zerschnipseln (Form?)
- Topologie (Triangulation)
- Lernmuster in doppelt so große Schnipsel zerlegen
- Schnipsel nach (image -: edge -: sum) bewerten
- Welche, die nur schwarz sind, rauswerfen
- Alle Schnipsel von allen Lernmustern zu einem Artikel in ein BOW schmeissen.
- Bildschnipsel darauf matchen
- Was ist mit der topologie? Hat die LP-BOW auch eine?
- Im Pharma könnte man die Schnipsel in uniflächen trennen
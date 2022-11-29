#Ceci est une version modifiée du script de Stefano Coretta s
#L'original est trouvable dans le package speakr version 3.2.1
#Ou sur son GitHub : https://github.com/stefanocoretta/speakr/blob/main/inst/extdata/plot.praat
#J'en profite pour le remercier lui et les personnes qui ont bossé sur speakr ! 

#Cette version modifiée plot l'intensité au lieu du pitch
#Utile pour la thèse de Rémi Anselme

form Select data
  comment Output file name:
  sentence file plot.png
  comment Set path to wav audio file and TextGrid:
  sentence wav ~/
  sentence tg ~/
  comment Start and end time to be plotted:
  real start 0
  real end 0
  comment Output file settings:
  positive width 5
  sentence format png
  comment Include f0 track?
  sentence pitch FALSE
  comment f0 minimum and maximum:
  real pitch_min 300 #A priori ca ne sert plus
  real pitch_max 500 #A priori ca ne sert plus
  comment Hz maximum:
  real hz_max 5000
endform

# Set dimensions ####################################################
# All dimensions are in inches.

# Width and height of the plotting window (not of the whole plot).
win_width = width - (0.7 * 2)
win_height = 2
# Height of the waveform panel.
wav_height = 0.5
# Height of the spectrogram panel.
sp_height = 1.5

# Read files ########################################################

wav = Read from file: wav$

if end == 0
  end = Get end time
endif

# Set plotting window dimensions if f0 will be included.
if pitch$ != "FALSE"
  pitch_height = 0.25
  sp_height = sp_height - 0.25
else
  pitch_height = 0
endif

# Set plotting window dimensions if textgrid will be included.
if tg$ != "FALSE"
  tg = Read from file: tg$
  tg_tiers = Get number of tiers
  tg_height = win_height + (0.5 * tg_tiers)
endif

# To objects ########################################################

selectObject: wav
sp = To Spectrogram: 0.005, hz_max, 0.00001, 20, "Gaussian"
#Jai change la troiseme valeur

if pitch$ != "FALSE"
  selectObject: wav
  pitch_obj = To Intensity: 300, 0, "yes"  
  #Jai changé le pitch par Intensity et j'ai mis 300
  selectObject: pitch_obj
  int_max = Get maximum: 0, 0, "Parabolic"
  
  if int_max >= 90
  	int_max = 90
  endif
  int_min = int_max - 40
#Jai rajoute quelques lignes pour avoir les valeurs de int a plotter

endif

# Set up plotting params ############################################

Erase all

Font size: 8
Helvetica

# Plot ##############################################################

### Spectrogram #####################################################

Select inner viewport: 0, win_width, wav_height, wav_height + sp_height

selectObject: sp
Paint: start, end, 0, 0, 100, "yes", 70, 6, 0, "no" 
#Jai change le no par un yes
Draw inner box

### f0 track ########################################################

if pitch$ != "FALSE"
  Select inner viewport: 0, win_width, wav_height + sp_height, wav_height + sp_height + pitch_height

  selectObject: pitch_obj
  Line width: 3
  Red
  #Jai change la couleur
  Draw: start, end, int_min, int_max, "no"
  #Jai change les valeurs
  Line width: 1
  Black
  Axes: start, end, int_min, int_max
  #Jai change les valeurs
  Marks right: 2, "yes", "yes", "no"
endif

### Spectrogram box #################################################

Select inner viewport: 0, win_width, wav_height, wav_height + sp_height

Draw inner box

### TextGrid ########################################################

if tg$ != "FALSE"
  Select inner viewport: 0, win_width, 0, tg_height
  selectObject: tg
  Draw: start, end, "yes", "yes", "no"
  inner_height = tg_height
else
  inner_height = win_height
endif

### Waveform ########################################################

Select inner viewport: 0, win_width, 0, wav_height

selectObject: wav
Black
Draw: start, end, 0, 0, "no", "Curve"
Black
#Jai change la couleur

### Axes ############################################################

int_min = Get minimum: start, end, "sinc70"
int_max = Get maximum: start, end, "sinc70"

Axes: start, end, int_min, int_max
One mark right: int_min, "no", "yes", "no", fixed$(int_min, 2)
One mark right: 0, "yes", "yes", "yes", ""
One mark right: int_max, "no", "yes", "no", fixed$(int_max, 2)

Select inner viewport: 0, win_width, wav_height, wav_height + sp_height

Axes: start, end, 0, hz_max
Marks left: 2, "yes", "yes", "no"
Text left: "no", "Hz"

Select inner viewport: 0, win_width, 0, inner_height

Draw inner box
Marks bottom every: 0.5, 0.05, "yes", "yes", "no"
Text bottom: "yes", "Temps (s)"

### Select plot and save ###########################################

Select inner viewport: 0, win_width, 0, inner_height

if format$ == "png"
  Save as 600-dpi PNG file: file$
elif format$ == "pdf"
  Save as PDF file: file$
else
  writeInfoLine: "Format not recognised."
endif

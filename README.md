# AutoCAD-LISP

## How to install
1. Temporary - simply drag .lsp files to ACAD window one by one. Will reset after relaunch.
2. Permanently - add to the Startup
- Place .lsp files to any Trusted Location to avoid popup windows at the start of the program (Options/Files/Trusted Locations, for example C:\Program Files\Autodesk\AutoCAD 2020\Plugins)
- Run the APPLOAD (Command).
- Under Startup Suite, click the Contents button.
- Click the Add button.
- Browse to the location of the LISP file(s), select, then click the Open button.

## Как установить
1. Временно - просто перетащить файлы .lsp в окно ACAD по одному. После перезапуска программы нужно добавлять заново
2. Постоянно - добавить в автозагрузку
- Поместить файлы .lsp в любое доверенное расположение, чтобы избежать всплывающих окон при запуске программы (Настройки/Файлы/Доверенные расположения, например C:\Program Files\Autodesk\AutoCAD 2020\Plugins)
- Команда APPLOAD,
- В разделе Автозагрузка нажмите кнопку Приложения.
- Нажмите кнопку Добавить.
- Перейдите к расположению файлов LISP, выберите их (можно несколько), затем нажмите кнопку Открыть.

[Autodesk/How to automatically load LISP routines in AutoCAD products](https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/Automatically-load-AutoLISP-routines.html)

## 3DFaceOutline
3DFACEOUTLINE command - draws a line along every 3DFACE object edge\
3DFACEBOTTOMLINE command - draws a line along bottom 3DFACE object edge

Команда 3DFACEOUTLINE - рисует линию вдоль каждой грани пластины (3DFACE)\
Команда 3DFACEBOTTOMLINE - рисует линию вдоль нижней грани пластины (3DFACE)

![3DFaceOutline](https://github.com/user-attachments/assets/e7008023-82aa-4a76-ab14-47fc5bd2489f)

## BatchLengthen
BATCHLENGTHEN command - moves both ends of a line (LINE, PLINE, 3DPOLY) along the line for defined distance

Команда BATCHLENGTHEN - перемещает оба конца линии (LINE, PLINE, 3DPOLY) вдоль этой линии на заданное расстояние

![BatchLengthen](https://github.com/user-attachments/assets/fcc4f6a3-7a43-457a-b2a3-4bf62816b8fe)

## BlockRename

BLOCKRENAME command - renames selected block, for multiple blocks - with numeric suffixes (_001, _002, etc.)

Команда BLOCKRENAME - переименовывает выбранный блок, если выбрано несколько - с окончаниями (_001, _002, и т.д.)

![BlockRename](https://github.com/user-attachments/assets/798e9954-e081-40c0-b08c-1190f382762c)

## CoordRound

COORDRND command - rounds coordinates of selected objects relative to a defined point (0,0,0 point as default)

Команда COORDRND - округляет координаты выбранных объектов относительно выбранной точки (точка 0,0,0 по умолчанию)

![CoordRound](https://github.com/user-attachments/assets/c93bd3cf-c37a-45a5-a285-eb719008bf81)

## DTCenterAlign

DTCENTER command - places selected text object at the center of multiple selected lines

Команда DTCENTER - размещает выбранный текст по центру и вдоль выбранных линий

![DTCenterAlign](https://github.com/user-attachments/assets/35e45fff-5994-486a-a2a5-f544d1554d3a)

## ReplaceWithBlock

REPLACEWITHBLOCK or RWB command - creates a block from selected objects and deletes them

Команда REPLACEWITHBLOCK или RWB - помещает выбранные объекты в блок и удаляет исходные

![ReplaceWithBlock](https://github.com/user-attachments/assets/8717589e-a3c7-4538-8c87-9826131c4f4f)

## Weld_3PointTriangle

WELD3T command - to draw custom angle weld symbol by 3 points

Команда WELD3T - рисует символ сварки (в разрезе) произвольной формы по 3 точкам

![Weld_3PointTriangle](https://github.com/user-attachments/assets/5133f328-91bc-473c-a5fc-2ce7d75a0989)

## Weld_RightTriangle

WELDRT command - to draw 90° weld symbol by 2 points

Команда WELDRT - рисует символ сварки (в разрезе) в форме прямоугольного треугольника по 2 точкам

![Weld_RightTriangle](https://github.com/user-attachments/assets/f803f5aa-e18a-4e16-b32c-5ca850ee097f)

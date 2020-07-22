# in|filtration

This repository contains materials, such as sketches, code, or [Mellite](https://sciss.de/mellite) workspaces,
relating to our sound installation piece [in|filtration](https://www.researchcatalogue.net/view/711664/711665)
and its research process.

All materials in this repository (C)opyright 2019&ndash;2020 Hanns Holger Rutz. All rights reserved.
Unless otherwise specified, source code is released under the [GNU Affero General Public License v3+](http://www.gnu.org/licenses/agpl-3.0.txt),
 whereas Mellite workspaces (including code snippets) are released under Creative Commons Attribution-ShareAlike 4.0 International
([CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/)) license.

---

## How to open a Mellite workspace

Make sure you have the latest version of _Mellite_ - you can download it from [here](https://archive.org/download/Mellite) - 
instructions are [here](https://www.sciss.de/mellite/#download-and-run). Read the instructions also for information on SuperCollider installation.

If have extracted the Mellite _zip_ download, you should be able to launch Mellite from the _bin_ directory. It takes a moment to come up. 
Once the menu appears, you can then open the workspace that you have downloaded separately (extract the _.mllt.zip_ archive to obtain 
the _.mllt_ directory/database). Select _File > Open_ and go into the parent directory of the _.mllt_ workspace, here select the _.mllt_
directory (do not double click), then select the _Open_ button.

## Running Negatum

Build using `sbt assembly`.

When using `run.sh`, a RAM disk must have been created:

    mkdir /tmp/ramdisk
    chmod 777 /tmp/ramdisk
    sudo mount -t tmpfs -o size=256m myramdisk /tmp/ramdisk

Options are something like

    --workspace /data/projects/Infiltration/workspaces/NegatumTrunk11.mllt --template /data/projects/Infiltration/audio_work/trunk11/trunk_47e8301c-%d-hilbert-curve.aif --end-frame 500 --gen-population 800

## Ordering

- negatum
- `java -Djava.io.tmpdir=/tmp/ramdisk -jar Mellite.jar -r Tweak,Select --headless NegatumTrunkNs.mllt`
- `java -jar Infiltration.jar copy-folder --in NegatumTrunkNs.mllt --out TrunkNpar.mllt --folder out`
- optimize
- parametrize
- remove-simple
- copy `MakeChains-empty` into workspace that has `par` ("TrunkNparC.mllt"); set `par` in the controls
- `java -Djava.io.tmpdir=/tmp/ramdisk -jar Mellite.jar -r CorrGroup --headless TrunkNparC.mllt`
- make-chains (no 'tie')
- `java -Djava.io.tmpdir=/tmp/ramdisk -jar Mellite.jar -r CorrEnds --headless TrunkNparC.mllt`
- make-chains `--tie` (play around with `--tie-strength` and `--tie-distance` if tying fails; try `--tie-distance 0.01`
  first, if still failing, begin increasing `--tie-strength 2.0 --tie-distance 0.02` etc.)

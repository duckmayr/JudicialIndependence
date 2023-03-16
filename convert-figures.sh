#!/usr/bin/env bash
cp map.pdf figures/
for f in figures/*.pdf; do pdftops -eps "$f"; done
mkdir figures-eps
mv figures/*.eps figures-eps/


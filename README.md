# VigilantBSP

## Author
(c) 2022 VigilantDoomer

## Description

VigilantBSP is a multi-threaded polyobject-aware
node / blockmap / reject builder (also called "nodebuilder" for 
brevity) for Doom / Doom 2 / Heretic / Hexen. 

It is being developed for Windows and GNU/Linux operating system on
both x86 (32-bit) and x86-64 (64-bit) platforms.

## Project goals

VigilantBSP intends to fulfill several ambitious goals:

1. Fast build times for maps without sacrificing support for 
special effects like self-referencing sectors. Use parallelism
where possible to achieve faster builds on multi-core computers.
2. Support for Heretic and Hexen games being as robust as support
for Doom / Doom 2.
3. Features that help get maps within vanilla limits are researched
and implemented, without getting in the way of support for maps 
targeting advanced engines (Boom, etc.)
4. Make self-referencing sector effects easy to use for mappers.
Default settings should already support self-referencing sector
effects very well. Monsters in self-referencing sectors shall
be able to see, hear and attack the player.
5. Special effects provided by other nodebuilders (horizon effect
and "precious" linedefs from BSP v5.2, faster scrollers from 
ZokumBSP) are also getting implemented.

In future, support for GL nodes / UDMF format MAY be considered
as well. 

Also, support for RMB effects is planned soon.

## Debt of gratitude

VigilantBSP is indebted to other free/libre software nodebuilders
for their ideas and implementations, and intends to give back its
own ideas as well. This is achieved by using a free software license.

The list of nodebuilders and people whose work make it all possible
includes, but is not limited to:
DEU by Raphael Quinet, 
BSP v5.2 by Colin Reed, Lee Killough and other contributors to BSP (program),
ZDBSP by Marisa Heit, 
Zennode by Marc Rousseau,
Zokumbsp by Kim Roar Foldøy Hauge, et al
AJ-BSP by Andrew Apted, et al

## License
VigilantBSP is free software: you can redistribute it
and/or modify it under the terms of GNU General Public License
as published by the Free Software Foundation, either version 2 of
the License, or (at your option) any later version.

VigilantBSP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with VigilantBSP (see file COPYING.txt). If not, see
<https://www.gnu.org/licenses/>.

## Usage

```
Usage: vigilantbsp {-options} filename.wad {-o output.wad}

See the documentation file README.txt for more information.

```

## Website / contact information

You can contact me (the developer) through a forum:
<https://thegreatresist.freeforums.net>
username: vigilantdoomer displayname: Vigilante

Discussion of nodebuilder happens currently in this
thread: 
<https://thegreatresist.freeforums.net/thread/7/vigilantbsp-v0-multi-threaded-nodebuilder>

If not, see subforum "Doom (series), Heretic, Hexen" and look for thread that has 
"VigilantBSP" in its name.

And of course you can open an issue on github.

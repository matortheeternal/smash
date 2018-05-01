# smash
A tool for performing automatic conflict resolution between multiple Bethesda Plugin Files for TES and Fallout games.

## explanation
Characters, items, quests, music tracks, leveled lists, weathers, magical effects, etc. are all represented in plugin files as records.  When multiple mods modify the same records, only the last loaded mod's changes get used by the game (with some exceptions).  This is known as the rule of one.  Many plugin patches combine the changes of multiple mods to resolve conflicts, but there is no way to create and distribute patches for every possible combination of conflicting mods.  This is where tools like Mator Smash come in.  Mator Smash allows for conflict resolution patches to be generated following rules, known as "Smash Settings".  This allows users to quickly and efficiently combine the edits of multiple mods.

## intended usage
Use Mator Smash to generate a patch, then verify the changes in the patch in xEdit.  Fix any conflicts that were not resolved correctly.  Smashed patches should be usable without a manual verification step, but can in certain rare cases lead to crashes/unintended behavior in game.  If you run into issues please report them [here](https://github.com/matortheeternal/smash/issues).  You can safely disable or delete your smashed patch at any time.

# resources
If you're looking for support or want to contribute, join the [Modding Tools discord server](https://discord.gg/GUfRdpT).

You can find additional information and discussion on the [Nexus Mods](https://www.nexusmods.com/skyrim/mods/90987) mod page, in the [STEP forum topic](http://forum.step-project.com/topic/6936-wip-mator-smash/), and in the [Nexus Mods forum topic](https://forums.nexusmods.com/index.php?/topic/3707015-wip-mator-smash-the-conflict-resolution-revolution/).
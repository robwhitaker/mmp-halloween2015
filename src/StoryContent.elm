module StoryContent where

import InteractiveStory.StoryBlock exposing (..)
import InteractiveStory.Action exposing (..)
import AnimationWrapper as AW
import Debug
import Html
import InteractiveStory.Sound as Sound
import InteractiveStory.VariableModel as VM

fadeIn = Sound.fade 0 1 1500
fadeOut = Sound.reverseTransition fadeIn

stuff =
  [ block01, block02, block03, block04, block05, block06, block07] ++ countdown_block08 ++ [block09, block10
  --contentBlock "Start - {{ding}}"
  --  |> (\b ->
  --      { b |
  --          onEnter <- (always { emptyEffectSet | variableEdits <- [UpdateString "ding" <| \str -> Just (Maybe.withDefault "" str)], soundUpdates <- [Sound.bgm "mansion-bgm" (Just fadeIn) (Just <| Sound.reverseTransition (Sound.fade 0 0.4 5000))] }),
  --          onLeave <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set second"] }),
  --          label <- Just "start"
  --      }
  --  )
  --, contentBlock "Block 2 - {{ding}}"
  --    |> (\b ->
  --          { b |
  --              onEnter <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set third"], soundUpdates <- [Sound.bgm "portal-bgm" (Just (Sound.fade 0 0.4 5000)) (Just fadeOut)] }),
  --              onLeave <- (always { emptyEffectSet | variableEdits <- [SetString "ding" "ding set fourth"], soundUpdates <- [Sound.StopSound "sound1"] })
  --          }
  --      )
  --, contentBlock "This is a string... {{ding}}" |> \b -> { b | label <- Just "goodbye" }
  --, choiceBlock "Bleh - {{ding}}" [("Go to hello", Just "hello", Nothing, Nothing), ("Go to goodbye", Just "goodbye", Nothing, Nothing), ("Go to start", Just "start", Just (always { emptyEffectSet | variableEdits <- [SetString "ding" "bAck to 1!", SetBool "beenToEnd" True]}), Just (VM.getBool "beenToEnd" False >> not))] True
  --, { emptyStoryBlock | contentGenerator <- \_ _ _ -> Html.text "hello!", label <- Just "hello" }
  ]

label l b = {b | label <- Just l }

onEnter f b = {b | onEnter <- f}

bgm l fi fo = (always {emptyEffectSet | soundUpdates <- [Sound.bgm l fi fo]})

stopBGM fo = (always {emptyEffectSet | soundUpdates <- [Sound.StopLoop Sound.BGM fo]})

block01 =
  """
Arlene stands in front of in you the grand hall staring with wide, sparkling eyes at the portal. Her fists are curled in front of her like a boxer preparing for a fight, and she is practically bouncing on the balls of her feet so that she looks just like a little kid eagerly awaiting a treat.

"Oh, this is so exciting!" she exclaims, looking from you to the portal as if she doesn't know where to go next. "Let's go pick out some costumes. Quickly!"

She grabs you by the wrist so tightly it hurts just a little, and drags you off into the mansion behind her. The halls wind and twist, and at some point you go down a set of stairs. Finally, she pushes the door to a room open, flicks on the light, and rushes inside.
""" |> contentBlock |> onEnter (bgm "mansion-bgm" (Just (fadeIn)) (Just fadeOut))

block02 = choiceBlock
  """
The walls are deep and lined with colorful costumes, some dainty, others strange, and others just plain gross, as Marissa may put it (endearingly, of course). Arlene hurries down the aisle of costumes plucking one out here, another there, picking one up and putting it back, then scooping another into her arms. When she returns to you, she has three costumes lined up.

"Oh, Reader, I can't decide? Which costume should I pick?"
"""
  [ ("Pick the witch costume with the black robe, pointy hat, and green witch's mask - big, warty nose included."
    , Just "arlene-costume-picked"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "arlene-costume" "witch"]})
    , Nothing
    ),
    ("Pick the Fairy Godmother costume with the sparkly, blue dress, the star-shaped wand, and the grey wig."
    , Just "arlene-costume-picked"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "arlene-costume" "fairy-godmother"]})
    , Nothing
    ),
    ("Pick the Gandalf costume with the grey wizard robe, giant staff, and flowing grey beard."
    , Just "arlene-costume-picked"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "arlene-costume" "gandalf"]})
    , Nothing
    )
  ]
  True

block03 =
  """
"A wonderful choice! Excuse me while I go change. You should pick a costume for yourself while I'm gone!"

With that she, vanishes into the changing room in the back that was previously hidden behind a towering Godzilla costume that might actually be made to scale.
""" |> contentBlock |> label "arlene-costume-picked"

block04 = choiceBlock
  """
You wander around the room, knowing there is no hope of looking through *every* costume in here. There are just far too many piled, hung, and boxed around the room. Still, you *should* pick something.
"""
  [ ("Dress as a knight with a shining suit of armor and a sword."
    , Just "reader-costume-picked"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "reader-costume" "knight"]})
    , Nothing
    ),
    ("Dress as a vampire with a flowing black cape, a swauve popped collar, some slicked back hair, and a nice set of rather pointy chompers."
    , Just "reader-costume-picked"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "reader-costume" "vampire"]})
    , Nothing
    ),
    ("Try to wear the Godzilla costume."
    , Just "reader-costume-picked"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "reader-costume" "godzilla"]})
    , Nothing
    ),
    ("Put on that ugly, contorted mask in the corner that probably would have given you nightmares as a kid."
    , Just "reader-costume-picked"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "reader-costume" "ugly-mask"]})
    , Nothing
    ),
    ("Don't wear a costume. You're too cool to dress up for Halloween."
    , Just "reader-costume-picked"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "reader-costume" "none"]})
    , Nothing
    )
  ]
  True

block05 =
  conditionalTextBlock
  [ (.string, "arlene-costume", "witch", """
When Arlene comes out of the changing room, you hardly recognize her underneath the draping, black robe and the wart-ridden, sickly green mask in front of her face. Her red hair puffs out in the back and frames her head underneath the brim of her pointed hat.

"What are ye doin' in the house of a witch, my pretty?" She cackles menacingly, throwing her head back. "Be gone with ye, quickly, into the changing room before I turn ye into a frog!" Another round of laughter as you walk past her into the room in the back to get changed.
""")
  , (.string, "arlene-costume", "fairy-godmother", """
Arlene comes out of the changing room in a beautiful blue dress. In one hand, she daintily carries a wand between two fingers. In the other, she is draping a keychain of a pumpking carriage that you're pretty sure did not actually come with the costume.

"Oh, my dear," she says, seeing you holding your costume. "You're going to be late for the ball! Quickly, into pumpkin carriage!" She tosses you the keychain. "Off to the changing room, now. Tut, tut!"

And off you go, to change into your own costume before heading out.
""")
  , (.string, "arlene-costume", "gandalf", """
Arlene comes out of the changing room garbed in a grey cloak with a comedically lucious beard dangling off her face. In one hand, she holds a wooden staff. You make for the changing room yourself, but she steps in front of you.

"YOU SHALL NOT PASS!" she exclaims, jabbing her staff loudly into the ground. "Just kidding!" She steps aside, smiling under the beard, which apparently tickles her nose because you hear her sneeze as you enter the changing room.
  """)
  ] |> label "reader-costume-picked"

block06 =
  conditionalTextBlock
  [ (.string, "reader-costume", "knight", """
You leave the changing room fully garbed as a knight of the round table... or some table at least. Arlene claps her hands together happily.

"Oh, my! How dashing you look, Sir Reader! If we come across any dragons in our quest for sugar, surely you will protect this fair maiden!" She raises a hand to her forehead in mock faint and laughs wildly."
"""),
  (.string, "reader-costume", "vampire", """
You leave the changing room with your fangs firmly in place and your cape trailing out behind you.

"Oh, no, Count!" she exclaims. "Please don't suck my blood. Think of the children... and take them first!" She laughs wildly.
"""),
  (.string, "reader-costume", "godzilla", """
You barely get the costume into the changing room to begin with and get stuck in the door on the way out. Arlene is too busy laughing to comment.
"""),
  (.string, "reader-costume", "ugly-mask", """
You walk out of the changing room in your normal clothes -- well, normal except for the grotesquely deformed mask you are now wearing. Arlene looks at you quizzically for a moment.

"Are you not going to wear a costume? Why did you take off your mask?" For a moment, she manages to keep her puzzled expression, but then she bursts into laughter.
"""),
  (.string, "reader-costume", "none", """
You walk out of the changing room again, unsure of why you bothered going in in the first place. It's not like you were actually changing. A moment passes, and you wonder if Arlene is going to be disappointed in you. However, when she sees you, she seems absolutely delighted.

"Goodness, Reader! You have chosen a truly terrifying costume. Where did you get that *mask*?" She shudders dramatically before bursting into wild laughter.
""")
  ]

block07 =
  """
When Arlene regains her composure, she leads you back to the grand hall where you both stand before the Halloween rift.

"Oh, one more thing!" Arlene says. "Wait here." She runs off into the hallway and comes back a minute later with two bags, one with a pumpkin on the front and the other with a black cat. She hands you the one with the pumpkin. "Okay, *now* we're ready. On three, okay?"
""" |> contentBlock

countdown_block08 = [
  "\"One...\"" |> contentBlock |> onEnter (stopBGM (Just fadeOut)),
  "\"Two...\"" |> contentBlock,
  "\"Two and a half...\"" |> contentBlock,
  "\"...\"" |> contentBlock,
  "\".....\"" |> contentBlock
  ]

block09 = """
THREE!

She grabs your arm as she dives head-first into the swirling mass of the portal, pulling you in along side her.
""" |> contentBlock |> onEnter (bgm "portal-bgm" Nothing Nothing)

block10 = "Inside the portal" |> contentBlock

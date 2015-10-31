module StoryContent where

import InteractiveStory.StoryBlock exposing (..)
import InteractiveStory.Action exposing (..)
import AnimationWrapper as AW

import Html
import InteractiveStory.Sound as Sound
import InteractiveStory.VariableModel as VM
import Dict
import Markdown
fadeIn = Sound.fade 0 1 1500
fadeOut = Sound.reverseTransition fadeIn

stuff =
  [ block_pre_01, block_pre_02, block01, block02, block03, block04, block05, block06, block07]
  ++ countdown_block08 ++
    [
      block09, block10, block11, block12, block13, block14, block15, block16,
      block17, block18, block19, block20, block21, block22, block23, block24, block25, block26, block27, block28, block29, block30, block31, block32, block33, block34, block35, block36, block37, block38, block39, block40, block41, block42, block43, block44, block45, block42_2, block42_3, block46, block47, block48, block49, block50, block51, block52, block53, block54
      , gameover, goHome, goHomeConfirmed, bundleOfFun
    ]

label l b = {b | label <- Just l }

onEnter f b = {b | onEnter <- f}
onLeave f b = {b | onLeave <- f}

bgm l fi fo = (always {emptyEffectSet | soundUpdates <- [Sound.bgm l fi fo]})

stopBGM fo = (always {emptyEffectSet | soundUpdates <- [Sound.StopLoop Sound.BGM fo]})

goto l b = { b | next <- always (Label l)}

setVars varSettings = (always {emptyEffectSet | variableEdits <- varSettings })

isTrue key vars = Dict.get key vars.bool == Just True

add field amount = UpdateNum field (Maybe.withDefault 0 >> (+) amount >> Just )

conditionalTextChoiceBlock texts default choices showChosen =
  choiceBlock "" choices showChosen
  |> \b ->
    { b | contentGenerator <- (conditionalTextBlock texts default).contentGenerator }

block_pre_01 = choiceBlock """
### Begin
Marc has been gone for a little while now, and Arlene has taken to staring out the window again, much as how you first met her. Though it is hard to tell from behind, she seems rather bored, and now that no one else is around, she has allowed her shoulders to slump gloomily. As it is just about Halloween, perhaps she would like visit your world and go trick or treating, Reader. How about you invite her?
"""
  [ ("Invite Arlene.", Just "invite-arlene", Nothing, Nothing)
  , ("No, thanks.", Just "bundle-of-fun", Nothing, Nothing)
  ] True |> onEnter (bgm "mansion-bgm" (Just (fadeIn)) (Just fadeOut))

bundleOfFun = """
### End
Well, aren't you a bundle of fun? If you get bored while not trick or treating, perhaps you would like to refresh the page and start over... That or just [return to watching other people have fun](/).
""" |> contentBlock |> label "bundle-of-fun" |> \b -> { b | next <- always Stop }

block_pre_02 = """
A weightiness comes over your body, and you find yourself quite tangible within the room again. This time, Arlene does hear your footsteps and turns to face you, smiling brightly, if not a bit distractedly.

"Oh, look who it is!" she exclaims. "Didn't manage to sneak up on me this time, now did you?" A cackling laugh escapes her throat, and she reigns it back in. "So to what do I owe this pleasure?"

You ask her if she'd like to go trick or treating with you.

"In your world, you mean? That sounds so exciting! I'd love to! But how would we--"

As if in answer to her question, a thin line slices through the air. For a moment, it hangs there, suspended in the air like a piece of silk, and then it begins to tear open. Air is sucked in with a *whoosh* as the tear in space grows wider, swirling with purple and red and orange and black. Through it, you can hear echoes of "trick or treat," screaming and then laughter, and spooky music. You can hear Halloween.
""" |> contentBlock |> label "invite-arlene"

block01 =
  """
Arlene stares at the portal, eyes wide and sparkling. Her fists are curled in front of her like a boxer preparing for a fight, and she is practically bouncing on the balls of her feet so that she looks just like a little kid eagerly awaiting a treat.

"Oh, this is so exciting!" she exclaims, looking from you to the portal as if she doesn't know where to go next. "Let's go pick out some costumes. Quickly!"

She grabs you by the wrist so tightly it hurts just a little, and drags you off into the mansion behind her. The halls wind and twist, and at some point you go down a set of stairs. Finally, she pushes the door to a room open, flicks on the light, and rushes inside.
""" |> contentBlock

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
    ("Dress as a vampire with a flowing black cape, a suave popped collar, some slicked back hair, and a nice set of rather pointy chompers."
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
Arlene comes out of the changing room in a beautiful blue dress. In one hand, she daintily carries a wand between two fingers. In the other, she is draping a keychain of a pumpkin carriage that you're pretty sure did not actually come with the costume.

"Oh, my dear," she says, seeing you holding your costume. "You're going to be late for the ball! Quickly, into pumpkin carriage!" She tosses you the keychain. "Off to the changing room, now. Tut, tut!"

And off you go, to change into your own costume before heading out.
""")
  , (.string, "arlene-costume", "gandalf", """
Arlene comes out of the changing room garbed in a grey cloak with a comedically luscious beard dangling off her face. In one hand, she holds a wooden staff. You make for the changing room yourself, but she steps in front of you.

"YOU SHALL NOT PASS!" she exclaims, jabbing her staff loudly into the ground. "Just kidding!" She steps aside, smiling under the beard, which apparently tickles her nose because you hear her sneeze as you enter the changing room.
  """)
  ] "" |> label "reader-costume-picked"

block06 =
  conditionalTextBlock
  [ (.string, "reader-costume", "knight", """
You leave the changing room fully garbed as a knight of the round table... or some table at least. Arlene claps her hands together happily.

"Oh, my! How dashing you look, Sir Reader! If we come across any dragons in our quest for sugar, surely you will protect this fair maiden!" She raises a hand to her forehead in mock faint and laughs wildly.
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
  ] ""

block07 =
  """
When Arlene regains her composure, she leads you back to the grand hall where you both stand before the Halloween rift.

"Oh, one more thing!" Arlene says. "Wait here." She runs off into the hallway and comes back a minute later with two bags, one with a pumpkin on the front and the other with a black cat. She hands you the one with the pumpkin. "Okay, *now* we're ready. On three, okay?"
""" |> contentBlock

countdown_block08 = [
  "\"One...\"" |> contentBlock |> onEnter (stopBGM (Just fadeOut)),
  "\"Two...\"" |> contentBlock,
  "\"Two and a half...\"" |> contentBlock,
  "\"Two and three quarters...\"" |> contentBlock,
  "\"...\"" |> contentBlock
  ]

block09 = """
"THREE!"

She grabs your arm as she dives into the swirling mass of the portal, pulling you in alongside her. You tumble in head first, and the force of the portal hurls you onward so harshly that it feels as if it will turn you inside out. Streaks of purple and orange dart around you, flashing into existence in one instant, flitting playfully around your head, and blinking out into the blackness of the portal in the next. In the distance, you can see shooting stars, except the stars aren't moving at all. Galaxies fly past, and your ears pound with the utter, discordant loudness of the abyss as it pulls you through. You hear screaming and laughing and howling and growling, all fading in and out in a clashing choir of Halloween fun.

You turn to look for Arlene, and she is just barely ahead of you, tumbling and laughing, having the time of her life. A dark rift appears in the distance ahead and grows closer at an alarming rate. Through it, you can make out the dull grey pavement of a street, painted orange by the streetlights and perhaps a glowing jack-o-lantern. In just a moment, you will rocket out of the other end of the portal and crash into the hard ground. As the exit hurtles towards you, you envision yourself splattering against the pavement, and you have to close your eyes. Even without sight, you can feel the exit approaching, propelling you to your--
""" |> contentBlock |> onEnter (bgm "portal-bgm" Nothing Nothing)

block10 = "You land safely on your feet." |> contentBlock |> onEnter (stopBGM (Just <| Sound.reverseTransition (Sound.fade 0 0.9 1500)))

block11 = choiceBlock """
Mostly safely. Your head is spinning, and you feel rather nauseous after nearly being flipped inside out. Somehow, Arlene seems to be doing fine. Better than fine, in fact. She is beaming at you.

"That. Was. *Awesome!* Can we do it again?" How she isn't feeling ill, you do not know, but at least...

Her smile falters, and she slaps a hand to her mouth. She suddenly looks rather green. "On second thought..." she manages, glancing around. "Perhaps we should find a trash can." She takes off down the street. A couple minutes pass before she returns, running and out of breath.
  """
  [ ("\"Are you all right?\"", Just "arlene-got-sick", Nothing, Nothing)
  , ("\"What happened?\"", Just "arlene-got-sick-2", Nothing, Nothing)
  ] True

block12 = """
  "Oh, I'm quite fine now. Though, we may wish to avoid a certain cop tonight. Don't worry, though. His uniform will be... hard to miss. Ahaha."
  """ |> contentBlock |> label "arlene-got-sick" |> goto "post-arlene-got-sick"

block13 = """
  "Oh, nothing, nothing! Though, we may wish to avoid a certain cop tonight. Don't worry, though. His uniform will be... hard to miss. Ahaha."
""" |> contentBlock |> label "arlene-got-sick-2" |> goto "post-arlene-got-sick"

block14 = """
  "Anyway," she continues, "let's *not* go through that portal again." She smiles and inhales deeply. The screams and laughter are closer now, all around you. Jack-o-lanterns are strewn about lawns and porches. Every other house seems to have styrofoam graves sunken into the lawn or skeletons hanging from the trees. Bushes are festooned with orange lights and puffy spider webs. "It sure has been a while," Arlene says with a distinctly nostalgic note in her voice. "Well, no use dilly-dallying. Let's go trick or treating!"
""" |> contentBlock |> label "post-arlene-got-sick" |> onLeave (bgm "trick-or-treat-bgm" (Just <| Sound.fade 0 0.8 3000) Nothing)

block15 = conditionalTextChoiceBlock
  [ (.bool, "done-with-everything", True, """
You return to the T-shaped intersection with Arlene. She looks up and down all the streets and then back at you.

"Oh, dear. It seems there isn't much left to do. Perhaps it's about time to head back to the Party, wouldn't you say?"

Looking up and down the darkening streets as people shut their lights and less and less people pass by, you have to agree. It's about time to head home.

"That was quite fun, though, Reader! It's been a while since I went trick or treating, so thanks for taking me along!"
  """)
  , (.bool, "visited-intersection", True, """
You return to the T-shaped intersection with Arlene.

"Lead the way, Reader!"
  """)]
  """
You are in a T-shaped intersection. Ahead of you is possibly the most decorated street you have ever seen. Spooky music eminates from one of the houses down this way, and the road is absolutely flooded with trick or treaters, hustling and bustling about, crying "trick or treat" happily at every door they approach. The street to your right is much quieter, and you can make out a couple hushed sobs coming from that direction. Down the road to your left, it sounds like a couple of teens are snickering about something. Perhaps they are up to something mischievous.

Arlene looks up and down all three roads before turning to you. "Hmm... so many choices. I wonder which way we should go first."
  """
  [ ("Go straight ahead (to trick or treating)."
    , Just "straight-ahead"
    , Just (always {emptyEffectSet | variableEdits <- [UpdateNum "story-counter" (Maybe.map ((+) 1) >> Maybe.withDefault 0 >> Just)]})
    , Just (\vars -> Dict.get "visited-trick-or-treat" vars.bool /= Just True)
    )
  , ("Return to trick or treating."
    , Just "pick-a-house"
    , Nothing
    , Just (\vars -> Dict.get "visited-trick-or-treat" vars.bool == Just True && not (isTrue "done-with-everything" vars))
    )
  , ("Go to the right (to crying children)."
    , Just "right-crying-children"
    , Nothing
    , Just (\vars -> Dict.get "talked-to-kids" vars.bool /= Just True)
    )
  , ("Return to children."
    , Just "talk-to-kids"
    , Nothing
    , Just (\vars -> Dict.get "talked-to-kids" vars.bool == Just True && Dict.get "done-with-kids" vars.bool /= Just True)
    )
  , ("Go to the left (to snickering teens)."
    , Just "left-bullies"
    , Nothing
    , Just (isTrue "visited-bullies" >> not)
    )
  , ("Return to high schoolers."
    , Just "high-schoolers-gone"
    , Nothing
    , Just (\vars -> isTrue "visited-bullies" vars && not (isTrue "high-schoolers-gone" vars))
    )
  , ("Go home for the night."
    , Just "go-home"
    , Nothing
    , Just (.bool >> Dict.get "done-with-everything" >> Maybe.withDefault False >> not)
    )
  , ("Go home for the night."
    , Just "go-home-confirmed"
    , Nothing
    , Just (.bool >> Dict.get "done-with-everything" >> Maybe.withDefault False)
    )
  ] True
  |> label "intersection-1"
  |> onLeave (setVars [SetBool "visited-intersection" True])
  |> \b -> {b | onEnter <- (\vars -> setVars [SetBool "done-with-everything" <| List.foldl (&&) True <| List.map (flip isTrue vars) ["high-schoolers-gone", "done-with-kids", "done-with-old-lady", "visited-park", "visited-creepy-music-house", "visited-house-with-scarecrow", "visited-down-road"]] vars)}

block16 = """
You go straight ahead and enter the bustle of enthusiastic trick or treaters. Children are running up and down the street, nimbly dodging around you as if you are no more than a lamp post or a fire hydrant. Arlene ogles at a couple well-decorated houses and screams in delight as a giant spider drops out of a tree at a group of kids as they approach a house. Then she takes off, dragging you down the road alongside her towards the source of some spooky music and witches' cackles.
""" |> contentBlock |> label "straight-ahead"

block17 = conditionalTextChoiceBlock
  [(.bool, "visited-trick-or-treat", True, """
You and Arlene return to the street and survey the houses you haven't visited yet.

"Where should we go next?" she asks.
  """)]
  """
You follow the big-eyed Arlene down the street until she abruptly stops in front of you. If you weren't paying attention, you would have run right into her.

"Oh, my. All of these houses look so *interesting*. Well, most of them." She glances at an undecorated house with all the lights off, a little distastefully. "Which one should we visit first?"
"""
  [ ("Go to the simple looking house with a single pumpkin sitting on the stoop."
    , Just "old-lady-house"
    , Nothing
    , Just (\vars -> not (Dict.get "done-with-old-lady" vars.bool == Just True) && not (Dict.get "smashed-pumpkin" vars.bool == Just True) )
    )
  , ("Return to the elderly woman's house."
    , Just "return-to-old-lady-house"
    , Nothing
    , Just (\vars -> not ((Dict.get "done-with-old-lady" vars.bool) == Just True) && ((Dict.get "smashed-pumpkin" vars.bool) == Just True))
    )
  , ("Go to the house with the scarecrow out front holding a bowl of candy."
    , Just "house-with-scarecrow"
    , Just (always {emptyEffectSet | variableEdits <- [SetBool "visited-house-with-scarecrow" True]})
    , Just (.bool >> Dict.get "visited-house-with-scarecrow" >> Maybe.withDefault False >> not)
    )
  , ("Go to the house with the creepy music blaring and the shadow of a ghoul projected on the side."
    , Just "creepy-music-house"
    , Just (always {emptyEffectSet | variableEdits <- [SetBool "visited-creepy-music-house" True]})
    , Just (.bool >> Dict.get "visited-creepy-music-house" >> Maybe.withDefault False >> not)
    )
  , ("Go to the house with all of the lights out and no decorations. Arlene doesn't seem particularly interested in this one."
    , Just "dark-house"
    , Nothing
    , Just (\vars -> Dict.get "visited-boring-house" vars.bool /= Just True)
    )
  , ("Return to the unlit house."
    , Just "dark-house"
    , Nothing
    , Just (\vars -> Dict.get "visited-boring-house" vars.bool == Just True && Dict.get "visited-park" vars.bool /= Just True)
    )
  , ("Return to the spooky park."
    , Just "spooky-park"
    , Nothing
    , Just (\vars -> Dict.get "visited-park" vars.bool == Just True && (Dict.get "done-with-sonny-jim" vars.bool /= Just True || Dict.get "done-with-pumpkin-patch" vars.bool /= Just True))
    )
  , ("Continue down the road."
    , Just "continue-down-road"
    , Just (setVars [SetBool "visited-down-road" True])
    , Just (\vars -> Dict.get "visited-down-road" vars.bool /= Just True)
    )
  , ("Go back to intersection."
    , Just "intersection-1"
    , Nothing
    , Nothing
    )
  ] True |> label "pick-a-house" |> onLeave (setVars [SetBool "visited-trick-or-treat" True])


block18 = choiceBlock """
You approach the house with the scarecrow. It is holding a bowl of candy in its lap with a sign that reads, "Please take two. Happy Halloween!" Perched menacingly above the bowl is a half-decayed zombie hand. You have an idea what'll happen if you reach in for the candy.
"""
  [ ("Reach for the candy *(take two)*."
    , Just "post-candy-bowl"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "scarecrow-took-candy" "two", add "morality" 1, add "candy" 2]})
    , Nothing
    )
  , ("Reach for the candy *(take a handful)*"
    , Just "post-candy-bowl"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "scarecrow-took-candy" "handful", add "morality" (-1), add "candy" 10]})
    , Nothing)
  , ("No way I'm reaching into that. It'll spook me!"
    , Just "post-candy-bowl"
    , Just (always {emptyEffectSet | variableEdits <- [SetString "scarecrow-took-candy" "none"]})
    , Nothing
    )
  ] True |> label "house-with-scarecrow"

block19 = conditionalTextBlock [
  (.string, "scarecrow-took-candy", "two", """
You tentatively poke your hand into the candy bowl. Your hand lands on a couple of pieces of candy below the hovering zombie fingers, and you pick up two. Just as you are about convinced nothing is going to happen, the zombie hand clamps down on yours with a terrible growl from a little speaker on the side of the bowl. You snatch your hand back with your two pieces of candy while Arlene cackles with delight.

"Goodness, you are so obedient, Reader!" she says, moving towards the bowl herself. She digs her whole hand into the bowl and giggles as the undead claw grabs and growls at her. She pulls out an entire handful of candy and dumps it into her bag. Then she goes back for a second. "They did say 'Take two'." She smiles shamelessly.

You both head back to the street.
  """),
  (.string, "scarecrow-took-candy", "handful", """
You dig your whole hand into the candy bowl, and the zombie fingers clamp down on your own with a terrible growl from a speaker on the side of the bowl. When it lets go, you pull out a whole handful of candy and dump it into your bag.

"My, my, Reader. Such a rebel. It says to please take two." She casts you a mischievous smile and digs her whole hand into the bowl, giggling when the undead claw grabs and growls at her. Then, she returns for another handful. "There, that's two."

You both head back to the street.
  """),
  (.string, "scarecrow-took-candy", "none", """
You totally wimp out because you're scared of a plastic hand.

"Oh, please," Arlene huffs. "Don't be such a baby." She grins mischievously and digs her whole hand into the bowl, giggling when the undead claw grabs and growls at her. She even goes back for seconds. "What? It *did* say to take two."

You both head back to the street.
""")
  ] "" |> goto "pick-a-house" |> label "post-candy-bowl"

block20 = """
You approach the house with the creepy music playing. It is surrounded by a rusted metal fence, and no sooner have you stepped into the yard than the gate slams shut behind you. A devilish laugh echoes from a bush nearby, and a dense fog begins to spread over the ground around your feet.

"They really went all out, huh?" Arlene asks, entranced by the fog dancing around the base of the foam headstones.

You both follow the electric torch-lit path a little farther before a ghost dives out of a tree towards you and Arlene with a ghastly wail and grey ribbons trailing out behind it. Arlene feigns a scream and grabs you by the wrist. "Run, Reader, before it gets us!"

She dodges off the main path, dragging you with her onto the headstone littered lawn. As you catch your breath, a new sound comes from behind you. The grumbling roar of a chainsaw revving and finally starting up. You turn around with a gasp and see a towering masked figure behind you.

"Oh, no! He's going to dismember us!" Arlene cries. "Quickly, this way!"

The masked man chases you both around the lawn, down the side of the house, into the surprisingly well-decorated backyard, and you finally lose him around the front of the house again.

"Woo," Arlene pants. "I think we lost him." She beams at you. "Shall we collect our surivors' prize?"

She drags you to the front door and knocks without waiting for an answer. A short man with a hump back, lumpy forehead, snaggletooth, and muddy brown cloak greets you. "Ahh, I see you have survived ze trials," he says in a distinctly fake Transylvanian accent. "I suppose you vish for a revard, of sorts?"

"Yes, please!" Arlene says.

"Very well," the man responds. If you asked him for his name, 9 out of 10 says he'd respond with Igor. "Here you are." He tosses a couple goodie bags to both you and Arlene. "Now begone with you both, before I decide to use you in my... experiments." He laughs ominously.

You and Arlene head back to the street.
""" |> contentBlock |> label "creepy-music-house" |> goto "pick-a-house" |> onLeave (setVars [add "candy" 30])

block21 = choiceBlock """
You approach the house with the single pumpkin sitting on the stoop. As you get closer, you can see that not only is this pumpkin not carved, it has a shabbily drawn face on it. Though the face is smiling, the shakily drawn lines make it look almost creepy.

"Oh, dear. What a sorry pumpkin this is," Arlene says, surveying the thing. A devilish look passes over her face. "Say, Reader, would you like to smash it in the street? It's been a while since I've partook in some good, old-fashioned Halloween mischief."
"""
 [ ("Yeah, let's smash it! It'll look better that way, honestly."
   , Just "smashing-pumpkins"
   , Just <| setVars [SetBool "smashed-pumpkin" True, add "morality" (-7)]
   , Nothing
   )
 , ("No way! Someone decorated that. Well, kinda..."
   , Just "not-smashing-pumkins"
   , Just <| setVars [SetBool "smashed-pumpkin" False, add "morality" 5]
   , Nothing
   )
 ] True
 |> label "old-lady-house"

block22 = choiceBlock """
"Yeah, I guess you're right," Arlene says, a little abashed. "Ahh well."

She walks up the steps past the pumpkin and knocks on the door. You wait for about 30 seconds before Arlene knocks again, harder this time.

"Jeez," she says. "Their lights are on. Wonder what's taking so--"

The door creaks open, and an elderly woman is standing in the door, hunched over a cane. Despite how much difficulty she seems to be having moving, she still somehow managed to paint sloppy whiskers on her face and put on some black cat ears and a tail.

"Hello, dears," she croaks with a smile. "Happy Halloween!"

"Happy Halloween!" Arlene responds, cheerily.

"I'm sorry it took me so long to answer the door. Takes a while for me to get around these days." The old woman sighs. "It even took a lot just to draw a face on that pumpkin this year. My joints aren't quite what they used to be, you know. I do so wish I could carve it, like I used to. Ah, but I'm rambling again. Here, let me get you kids some treats."

She teeters as she pivots and reaches for a nearby bowl of candy. Arlene looks at you and whispers, "Thanks for not letting me smash that pumpkin. I would have felt *terrible*. Actually, do you think we could help this old black cat carve it?"

The old lady turns around again and drops a couple pieces of candy into both of your bags, her hands shaking.
"""
  [ ("\"Excuse me, ma'am. Would you like us to help you carve your pumpkin?\" *(carve pumpkin)*"
    , Just "carve-pumpkin", Just (setVars [add "morality" 5]), Nothing)
  , ("\"Thank you!\" *(just leave)*"
    , Just "pick-a-house"
    , Just <| setVars [SetBool "done-with-old-lady" True]
    , Nothing)
  ] True |> label "not-smashing-pumkins" |> onLeave (setVars [add "candy" 7])

block23 = """
Arlene picks up the pumpkin, and you follow her out into the street.

"Ready?" she asks. "One. Two. Three!"

She hurls the pumpkin as high as she can into the air. It flies above your head in a swooping arch and crashes with a satisfying *splat* on the road. Orange pumpkin guts explode from the seams, and the poorly drawn face splits and shatters across the road. A couple nearby parents regard you rather unfavorably and lead their kids farther away.

"Oh, no!" a reedy voice croaks from behind you. Looking back, you see an elderly woman standing in the doorway staring at the pumpkin you just smashed. You know, the one you stole from her house. "Why would you do that," she asks. She doesn't sound angry, just sad and resigned. "It was the only decoration I had..."

You're not sure if she's talking to you or to herself at this point, but she turns around, one shaky hand resting heavily on her cane, and returns to her house. A couple minutes later, the lights go out.

"Guess she doesn't feel like having more visitors," Arlene says with an uncomfortable laugh. "Actually, I feel kind of terrible now. Maybe we should get her another pumpkin."
""" |> contentBlock |> label "smashing-pumpkins" |> goto "pick-a-house"

block24 = choiceBlock """
You approach the old lady's house, glancing at the place her pumpkin used to be, and knock on the door. It takes a little while before anyone answers, but eventually she does make it to the door. It looks like she has unsuccessfully tried to wipe a set of painted whiskers off her face.

"What do you kids want from me now?"
"""
  [ ("Give her the pumpkin and apologize."
    , Just "gave-pumpkin"
    , Just <| setVars [SetBool "has-pumpkin" False, add "morality" 5]
    , Just (\vars -> vars.bool |> Dict.get "has-pumpkin" |> Maybe.withDefault False)
    )
  , ("Apologize."
    , Just "apologized"
    , Just (setVars [SetBool "apologized" True, add "morality" 3])
    , Just (\vars -> Dict.get "apologized" vars.bool /= Just True)
    )
  , ("Nothing. *(return to street)*", Just "pick-a-house", Nothing, Nothing)
  ] True |> label "return-to-old-lady-house"

block25 = """
"We're really sorry about what we did to your pumpkin. I feel terrible about it," Arlene says. You apologize in suit.

The old lady regards both of you for a moment. "Well, it's alright, I suppose," she finally says. "I just wish you'd thought about it *before* you did it. But thank you for the apology. Now if you'll excuse me, I would like to be alone, I think."

She hobbles back into her house and closes the door. You and Arlene return to the street.
""" |> contentBlock |> label "apologized" |> goto "pick-a-house"

block26 = choiceBlock """
"We're really sorry about what we did to your pumpkin," Arlene says. "But look!" She pulls the new pumpkin out of your bag and holds it up for the elderly woman. "We got you a new one!"

The old woman's eyes light up, and she smiles so genuinely that you can't help but feel like you did something really great -- even though this was your fault in the first place.

"What a surprise! Thank you, dears," she says. "It's lovely. Even better than the first." She looks from you to Arlene to the pumpkin. "I don't suppose you'd like to help an old woman carve it? My hands aren't quite what they used to be, I'm afraid."
"""
  [ ("\"We'd love to!\" *(carve pumpkin)*"
    , Just "carve-pumpkin"
    , Just <| setVars [add "morality" 5]
    , Nothing
    )
  , ("\"No thanks.\" *(return to street)*"
    , Just "pick-a-house"
    , Just <| setVars [SetBool "done-with-old-lady" True]
    , Nothing
    )
  ] True |> label "gave-pumpkin"

block27 = """
The old woman is delighted that you want to carve the pumpkin with her, and you follow her into her house. The pace going from the door to the kitchen is slow as she hobbles in front of you, but you eventually make it to the kitchen table where you place the pumpkin down. She directs you to a cabinet with some markers and another with knives and spoons to carve the pumpkin and scoop out the guts. Together, you design a pretty awesome looking pumpkin, and you begin to carve it.

"Dear, it just occurred to me that I haven't yet introduced myself," the old woman says. "My name is Lucille, but you can just call me Lucy. What are your names?"

"I'm Arlene," Arlene says, jabbing the knife into the pumpkin's face again and popping out an eye. "And you can call my friend Reader." She gestures at you.

"Arlene and Rita. Such pretty names. You know, I knew a little girl once who wanted her name to be Arlene. She even went around telling people it *was* her real name. I lost track of her after her mother passed, unfortunately. But she was always such a sweet girl."

"Oh, my. I do hope she's alright," Arlene says, sounding a little surprised. "Perhaps you'll meet her again someday."

"Perhaps," Lucy chuckles. "My, the pumpkin looks lovely."

You pop the mouth out of the pumpkin and clean up around the edges.

"Now all it needs is a candle!" Lucy gets to her feet with the help of her cane and shuffles across the kitchen. She fishes a candle and a match out of a nearby drawer, and Arlene helps her light it and place it inside the pumpkin.

You bring it outside where it flickers orange and smiles at the passing trick or treaters. Lucy is smiling too.

"Thank you, dears, for making this a wonderful Halloween," she says.

You and Arlene wish her well and head back to the street where you cast one final glance back at the pumpkin. It's not a masterwork, but it really brings some life to the house anyway.
""" |> contentBlock |> label "carve-pumpkin" |> goto "pick-a-house" |> onLeave (setVars [SetBool "done-with-old-lady" True])

block28 = conditionalTextChoiceBlock
  [ (.bool, "visited-boring-house", True, """
You return to the unlit house and can hear growling somewhere around the back. Arlene seems to want to check it out.
    """)
  ]
  """
You approach the boring, unlit house with Arlene dragging her feet behind you. She doesn't really want to visit this house since obviously no one is even home, but for some reason you insist. When you get to the door, you knock.

No one answers. I mean, what did you expect really? There aren't even cars in the driveway.

You turn around to leave, but before you get very far, you hear something growling in the distance. It sounds as if it's coming from somewhere behind the house. Arlene now looks quite interested.

"Perhaps coming here will be interesting after all!"
    """
  [ ("Turn back.", Just "pick-a-house", Nothing, Nothing)
  , ("Investigate the growling.", Just "dark-house-back-yard", Nothing, Nothing)
  ] True |> label "dark-house" |> onLeave (setVars [SetBool "visited-boring-house" True])

block29 = choiceBlock """
You walk around the back of the house with Arlene and follow the growling to a fence in the back yard. Beyond the fence, there seems to be some sort of creepy park. The growling is coming from in there somewhere.
"""
  [ ("Turn back.", Just "pick-a-house", Nothing, Nothing)
  , ("Hop the fence.", Just "hop-the-fence", Just (setVars [SetBool "visited-park" True]), Nothing)
  ] True |> label "dark-house-back-yard"

block30 = conditionalTextBlock
  [(.string, "reader-costume", "godzilla", """
Arlene hops the fence ahead of you, and you try to follow. However, since you decided to wear that ridiculously oversized Godzilla costume, you can't quite seem to make it over the fence. Eventually, Arlene gets tired of waiting and climbs back over to hoist you up. You barely clear the top of the fence and tumble over into the park. Embarrassing.

"Oh, my. Are you alright, Reader?" Arlene asks, landing nimbly beside you. She grabs your hand and helps you off the ground.
  """)]
  """
You and Arlene hop the fence and land in a strange park.
  """ |> label "hop-the-fence" |> goto "spooky-park"

block31 = choiceBlock """
The park is incredibly dark, lit only by the twinkling stars and pale moonlight. The growling seems to be coming from just ahead, possibly behind a gnarled tree. Under the tree, you can make out a figure in the moonlight. It looks like... a corpse? If it weren't Halloween, that might be really freaky. To your right, you can see a pumpkin patch, though you can't imagine why that would be in a park to begin with.
  """
  [ ("Investigate the corpse."
    , Just "park-corpse"
    , Nothing
    , Just (\vars -> Dict.get "done-with-sonny-jim" vars.bool /= Just True)
    )
  , ("Visit the pumpkin patch."
    , Just "pumpkin-patch"
    , Nothing
    , Just (\vars -> Dict.get "done-with-pumpkin-patch" vars.bool /= Just True)
    )
  , ("Turn back.", Just "pick-a-house", Nothing, Nothing)
  ] True |> label "spooky-park"

block32 = conditionalTextChoiceBlock
  [ (.bool, "smashed-pumpkin", True, """
You and Arlene head over to the pumpkin patch.

"How delightful!" Arlene exclaims, dancing around the fat orange pumpkins. "Oh, let's pick one for the old lady! We do kind of owe her a pumpkin, after all."
  """)
  ] """
You and Arlene head over to the pumpkin patch.

"How delightful!" Arlene exclaims, dancing around the fat orange pumpkins. "I do love pumpkin picking. Shall we pick ourselves a pumpkin, then?"
    """
  [ ("Pick a pumpkin."
    , Just "pumpkin-picked"
    , Just (setVars [SetBool "has-pumpkin" True, SetBool "done-with-pumpkin-patch" True])
    , Nothing
    )
  , ("Leave pumpkin patch."
    , Just "spooky-park"
    , Nothing
    , Nothing
    )
  ] True |> label "pumpkin-patch"

block33 = choiceBlock """
You and Arlene approach the corpse. It is indeed very realistic. And gruesome. It's face has been torn off.

You look up and see a big dog in front of you, growling and baring its gigantic, sharp teeth. It's wearing a name tag that says "Sonny Jim". Arlene finds this hilarious.

"Did some old geezer name this thing? 'Hey get off mah lawn Sonny Jim!' She makes a whistling sound with her tongue on the 'S' so it sounds like "Shunny". The dog gets closer, still growling.
"""
  [ ("Pet the dog. It's clearly friendly."
    , Just "game-over"
    , Just (setVars [
      SetBool "done-with-sonny-jim" True,
      SetString "game-over-text" "Congratulations. Sonny Jim ripped your face off.",
      SetString "game-over-text-2" "But you died. So there's that. And it wasn't even at the Party, so you don't get to tell a story. You just get to be dead. Woohoo?"
      ])
    , Nothing
    )
  , ("Run for your life!"
    , Just "run-from-sonny-jim"
    , Just (setVars [SetBool "done-with-sonny-jim" True])
    , Nothing
    )
  ] True |> label "park-corpse"

block34 = conditionalTextBlock
  [ (.string, "reader-costume", "godzilla", """
You and Arlene turn tail and try to escape Sonny Jim, but the big dog is fast and is gaining on you quickly, drool slobbering out of its mouth as it barks at you. Arlene laughs, screaming "Catch me if you can, puppy!" After running the Midnight Murder Party, she doesn't seem too fazed by the idea of being torn apart. You, on the other hand...

You two arrive at the fence to the park, and Arlene hops over no problem. You, however, forgot that you chose the most unwieldy costume ever and now realize that you can't make it over the fence. Sonny Jim pounces.
    """)
  ] """
You and Arlene turn tail and try to escape Sonny Jim, but the big dog is fast and is gaining on you quickly, drool slobbering out of its mouth as it barks at you. Arlene laughs, screaming "Catch me if you can, puppy!" After running the Midnight Murder Party, she doesn't seem too fazed by the idea of being torn apart. You, on the other hand...

You two arrive at the fence to the park and hop over. Sonny Jim is left barking at you from the other side. You return to the street.
  """
  |> (\b -> { b |
    next <- (\vars -> if Dict.get "reader-costume" vars.string == Just "godzilla" then Label "game-over" else Label "pick-a-house"),
    onLeave <- (\vars ->
      if Dict.get "reader-costume" vars.string == Just "godzilla"
      then { emptyEffectSet |
        variableEdits <-
          [ SetString "game-over-text" "Congratulations. Sonny Jim ripped your face off."
          , SetString "game-over-text-2" "But you died. So there's that. And it wasn't even at the Party, so you don't get to tell a story. You just get to be dead. Woohoo?"
          ]}
      else emptyEffectSet )
  }) |> label "run-from-sonny-jim"

block35 = """
Arlene searches the pumpkin patch, picking up pumpkins and examining them in hopes of finding the perfect one. Finally, she settles on a plump, nicely rounded pumpkin hidden in the back of the patch and stuffs it in your bag.

"Pumpkin in the pumkin bag!" she sings. You two leave the pumpkin patch.
""" |> contentBlock |> label "pumpkin-picked" |> goto "spooky-park"

gameover = """
### End

{{game-over-text}}

While trick or treating, you collected {{candy}} pieces of candy and were {{behavior}}.

{{game-over-text-2}}

If you want to hang out with Arlene some more, why not [head back to the Midnight Murder Party](/read.html)?

Or, if you want to try again, just refresh the page.
"""
  |> contentBlock |> label "game-over"
  |> \b ->
    { b |
      next <- always Stop,
      onEnter <- (\vars -> { emptyEffectSet |
        variableEdits <-
          let
            morality = Maybe.withDefault 0 <| Dict.get "morality" vars.num
            behavior = SetString "behavior" <|
              if | morality > -5 && morality < 5 -> "a pretty okay person"
                 | morality >= 5 && morality < 15 -> "a good person"
                 | morality >= 15 -> "a fantastic rolemodel for society"
                 | morality <= -5 && morality > -10 -> "a bit of a prankster"
                 | morality <= -10 && morality > -20 -> "extremely mischievous. Actually, you might owe some people apologies... Yeah..."
                 | morality <= -20 -> "a horrible person"
            candy = SetNum "candy" <| Maybe.withDefault 0 <| Dict.get "candy" vars.num
            gameovertext = SetString "game-over-text" <| Maybe.withDefault "" <| Dict.get "game-over-text" vars.string
            gameovertext2 = SetString "game-over-text-2" <| Maybe.withDefault "" <| Dict.get "game-over-text-2" vars.string
          in [behavior,candy,gameovertext,gameovertext2]
      })
    }

block36 = """
You continue walking down the street, stopping at various houses along the way and collecting plenty of candy. But the farther you get down the road, the less the houses seem to be decorated. Eventually, Arlene suggests that you head back to the fun part of the street, and you agree.

You turn back.
"""
  |> contentBlock
  |> label "continue-down-road"
  |> onLeave (setVars [add "candy" 200])
  |> \b -> { b |
    next <- (\vars -> if Dict.get "visited-bullies" vars.bool /= Just True then Label "bully-snatches-candy" else Label "pick-a-house") }

block37 = choiceBlock """
But before you get very far, a teen runs by, digs his hands into both your bag and Arlene's simultaneously, and snatches out huge handfuls of candy. By all rights, it was rather an impressive motion, but...

"Hey!" Arlene shouts. "That brat just stole our candy!"
"""
  [ ( "Chase him."
    , Just "chase-bully"
    , Just (setVars [SetBool "chased-bully" True])
    , Nothing
    )
  , ( "Let him go."
    , Just "let-bully-go"
    , Just (setVars [SetBool "let-bully-go" True])
    , Nothing
    )
  ] True |> label "bully-snatches-candy" |> onLeave (setVars [SetBool "bully-snatched-candy" True, add "candy" (-15)])

block38 = """
"Get back here you little punk!" Arlene yells, bolting down the street after him. She is surprisingly fast, and you can barely keep up. You two chase him down the block, through someone's yard, and manage to catch up with him as he rounds the corner. Arlene grabs his shoulder and yanks it back, sending the kid sprawling to the ground.

"What's your problem?" The teen spits. He must be at least seventeen and his expression is infuriatingly smug. Arlene looks at him like he's an idiot.

"What do *you* think. Drop it," she says, none too kindly.

The teen's expression falters. "Y-yeah? Make me."

Arlene sighs. "Well, if you insist..." She grabs his wrist and twists it behind his back.

"Ow, ow, ow! Let go of me! I'll give you the stupid candy."

"Oh, goodie!" Arlene says, releasing his arm and clapping her hands together.

The teen picks up some of the dropped candy from the ground and returns it to your bags, hesitantly glancing at Arlene as he does so. But she seems quite content again. Once the candy is all back where it belongs, he makes to leave, then pauses. "Uh... so, can I go?"

"Yes, indeed. Hopefully we won't have any more trouble tonight, okay?"

"Y-yeah," he responds. With one final glance back, he takes off down the road.

When he is out of sight, Arlene turns to you. "Phew... I forgot how tiring acting tough was." She chuckles and says, "Well, in any case, back to trick or treating!"
""" |> contentBlock |> label "chase-bully" |> goto "pick-a-house" |> onLeave (setVars [add "candy" 15])

block39 = """
"Get back here you little punk!" Arlene yells. But before she can chase after him, you put a hand on her shoulder and ask her to just let it go. She pouts petulantly but agrees.
""" |> contentBlock |> label "let-bully-go" |> goto "pick-a-house"

block40 = choiceBlock """
You head down the street to your right. Just ahead, there is a group of kids sitting on the curb. One or two of them are crying.

"I wonder what's happened to them," Arlene ponders. "I do hope they're alright."
"""
  [ ("Approach the crying children."
    , Just "talk-to-kids"
    , Nothing
    , Nothing
    )
  , ("Go back to intersection. You're sure they're fine."
    , Just "intersection-1"
    , Nothing
    , Nothing
    )
  ] True |> label "right-crying-children"

block41 = conditionalTextChoiceBlock
  [(.bool, "talked-to-kids", True, """
You head down the street to your right and walk up to the sad-looking children. They look up at you hopefully. "Have you found our candy?" the girl asks.
  """)
  , (.bool, "has-candy", True, """
You approach the sniffling children and ask what's wrong. A brown-haired girl who appears to be comforting her little brother looks up at you mistrustfully.

"What do you want? We don't have any candy left, if you're trying to steal it too."

"Someone stole your candy? It didn't happen to be a bunch of high schoolers, did it?" Arlene asks, glancing back towards where you met the bullies.

"Y-yeah," the younger boy sniffles. "It was a bunch of stupid-face big kids. We worked really hard getting that candy..."

Another kid, who has been quietly staring at you two in awe, speaks up. "So... t-they went that way... I think." He points back towards the intersection.

"I see," Arlene says. "We had a bit of a run in with them ourselves. Those guys were jerks."

"Yeah, you said it!" the brown-haired girl agrees.
""")
  , (.bool, "high-schoolers-gone", True, """
You approach the sniffling children and ask what's wrong. A brown-haired girl who appears to be comforting her little brother looks up at you mistrustfully.

"What do you want? We don't have any candy left, if you're trying to steal it too."

"Someone stole your candy?" Arlene asks, taken aback.

"Y-yeah," the younger boy sniffles. "It was a bunch of stupid-face big kids. We worked really hard getting that candy..."

Another kid, who has been quietly staring at you two in awe, speaks up. "So... t-they went that way... I think." He points back towards the intersection.

"Oh." Arlene says. "Ahaha. I think we ran into them earlier, but... we kind of lost track of them. Sorry."

"That's alright," the brown-haired girl says. She sounds rather disappointed. "Well, thanks anyways." She looks at her brother. "Stop moping, dummy. If we go now, we can still get some candy."
    """)
  ]
  """
You approach the sniffling children and ask what's wrong. A brown-haired girl who appears to be comforting her little brother looks up at you mistrustfully.

"What do you want? We don't have any candy left, if you're trying to steal it too."

"Someone stole your candy?" Arlene asks, taken aback.

"Y-yeah," the younger boy sniffles. "It was a bunch of stupid-face big kids. We worked really hard getting that candy..."

Another kid, who has been quietly staring at you two in awe, speaks up. "So... t-they went that way... I think." He points back towards the intersection.

"Perhaps we could retrieve your candy," Arlene says.

The sniffling kid looks up at her hopefully. "W-would you really?"

"Yup! We'll keep an eye out!"
"""
  [ ("Give candy."
    , Just "return-childrens-candy"
    , Just <| setVars [add "morality" 5, add "candy" 12, SetBool "has-candy" False]
    , Just (\vars -> Dict.get "has-candy" vars.bool == Just True)
    )
  , ("Give them some of your own candy."
    , Just "give-some-candy"
    , Just <| setVars [add "morality" 6, UpdateNum "candy" (Maybe.map (\n -> if n - 100 < 0 then 0 else n - 100))]
    , Just (\vars -> isTrue "high-schoolers-gone" vars && (Dict.get "candy" vars.num |> Maybe.withDefault 0) > 0 && not (isTrue "has-candy" vars) )
    )
  , ("Return to intersection."
    , Just "intersection-1"
    , Just (\vars -> if isTrue "high-schoolers-gone" vars && not (isTrue "has-candy" vars) then setVars [SetBool "done-with-kids" True] vars else setVars [] vars)
    , Just (\vars -> isTrue "has-candy" vars || Dict.get "talked-to-kids" vars.bool /= Just True)
    )
  , ("\"Sorry, not yet.\""
    , Just "intersection-1"
    , Nothing
    , Just (\vars -> Dict.get "talked-to-kids" vars.bool == Just True && not (isTrue "high-schoolers-gone" vars))
    )
  , ("\"About that...\""
    , Just "we-lost-them"
    , Nothing
    , Just (\vars -> isTrue "talked-to-kids" vars && isTrue "high-schoolers-gone" vars && not (isTrue "has-candy" vars))
    )
  ] True |> label "talk-to-kids" |> onLeave (setVars [SetBool "talked-to-kids" True])

block42_3 = choiceBlock """
You tell the kids that you think you found the high schoolers who took it, but you... well... kind of lost track of them.

"Ahaha, yeah. Sorry about that." Arlene says.

"That's alright," the brown-haired girl says. She sounds rather disappointed. "Well, thanks anyways." She looks at her brother. "Stop moping, dummy. If we go now, we can still get some candy."
"""
  [ ("Give them some of your own candy."
    , Just "give-some-candy"
    , Just <| setVars [add "morality" 6, UpdateNum "candy" (Maybe.map (\n -> if n - 100 < 0 then 0 else n - 100))]
    , Just (\vars -> (Dict.get "candy" vars.num |> Maybe.withDefault 0) > 0)
    )
  , ("Return to intersection."
    , Just "intersection-1"
    , Just (setVars [SetBool "done-with-kids" True])
    , Nothing
    )
  ] True |> label "we-lost-them"

block42 = """
"Look what we found!" Arlene sings, holding up several bags of candy before the children.

"Our candy!" The sniffling boy yells, jumping from the curb. "Thank you, miss!"

"Yeah, thanks a bunch," his older sister says, smiling.

The quiet kid nods appreciatively but says nothing.

"Our pleasure," Arlene says. She bows theatrically and hands the bags off to the kids.

"Here," the brown-haired girl says, extending a fistful of candy and dumping it into Arlene's bag and then dumping a handful into yours as well. "That's for being really cool!"

"Why, thank you!" Arlene says, surprised by the gift. "I hardly require compensation, but who can turn down candy?" She tips the girl a wink, and the younger girl giggles.

The kids say goodbye and run off down the street to continue trick or treating.
""" |> contentBlock |> label "return-childrens-candy" |> goto "intersection-1" |> onLeave (setVars [SetBool "done-with-kids" True])

block42_2 = """
You and Arlene both offer them some of your candy, which they accept happily.

"Thanks a bunch!" the brown-haired girl says, beaming.

"Yeah, thanks!" her younger brother yells, wiping the tears from his face.

The quiet kid nodes appreciatively but says nothing.

"Our pleasure," Arlene says. She bows theatrically. The kids run off down the block.
""" |> contentBlock |> label "give-some-candy" |> goto "intersection-1" |> onLeave (setVars [SetBool "done-with-kids" True])

block43 = choiceBlock """
You walk down the street to the left and see a group of teens up ahead. They are sitting on a curb, covered in shaving cream and snickering between mouthfuls of candy. None of them appear to be wearing costume, but they have three childish looking trick-or-treat bags between them. You get the feeling the bags aren't theirs.

"These kids look like trouble," Arlene says, grinning. "Shall we see what they're up to?"
"""
  [ ("Check it out."
    , Just "approach-bullies"
    , Nothing
    , Nothing
    )
  , ("No way! I don't mix with that type..."
    , Just "intersection-1"
    , Nothing
    , Nothing
    )
 ] True |> label "left-bullies"

block44 = choiceBlock ""
  [ ("Take back kids' candy."
    , Just "take-back-candy"
    , Nothing
    , Just (\vars -> isTrue "talked-to-kids" vars && isTrue "chased-bully" vars)
    )
  , ("\"Correct\" the situation. *(take back candy)*"
    , Just "correct-the-situation"
    , Just (\vars -> if isTrue "let-bully-go" vars then setVars [SetString "bully-ditches-your-candy" "", SetString "bully-ditches-your-candy" "The kid who stole your candy even yanks it out of his pocket and throws it at the ground in front of him."] vars else setVars [SetString "bully-ditches-your-candy" ""] vars)
    , Just (\vars -> isTrue "talked-to-kids" vars && not (isTrue "chased-bully" vars))
    )
  , ("Egg the school."
    , Just "egg-the-school"
    , Just <| setVars [add "morality" (-6)]
    , Just (\vars -> isTrue "chased-bully" vars || (not (isTrue "talked-to-kids" vars) && not (isTrue "bully-snatched-candy" vars)))
    )
  , ("Confront them about their potentially stolen candy."
    , Just "take-back-candy"
    , Nothing
    , Just (isTrue "talked-to-kids" >> not)
    )
  , ("Leave. *(They might not be here when you get back. They have important... high schooler things to do.)*"
    , Just "intersection-1"
    , Nothing
    , Nothing
    )
  ] True
  |> \b ->
    { b |
      contentGenerator <-
        (\_ vars _ ->
          let
            arleneWhisper = if isTrue "talked-to-kids" vars then block44_3 else block44_4
            bullyOffer = if isTrue "talked-to-kids" vars then block44_2 ++ block44_2_1 else block44_2
            secondaryResponse =
              if isTrue "bully-snatched-candy" vars
              then if isTrue "chased-bully" vars then bullyOffer else arleneWhisper
              else if isTrue "talked-to-kids" vars then block44_5 else block44_6
          in Markdown.toHtml <| block44_1 ++ secondaryResponse
        )
    } |> label "approach-bullies" |> onLeave (setVars [SetBool "visited-bullies" True])

block44_1 = """
You and Arlene approach the snickering high schoolers. Upon closer inspection, you are almost certain that their candy bags don't actually belong to them.

The high schoolers look up at you as you approach. "Pfft. Look at this dope," the blonde one in the front says, jerking a thumb towards Arlene. The patch on his Letterman jacket reads "Henry", and you assume he is the leader of the posse. "What is she, like 20, and still wearing a costume?" The others roar with laughter.

"Well, I never!" Arlene huffs.

"Relax, babe, just kidding around with you." The others hoot and holler at this.
"""

block44_2 = """Except for one, who slipped into the background when you and Arlene arrived. He now whispers something to Henry who listens to what his buddy says and gets to his feet, no longer smiling. "My friend here says you beat him up before and stole his candy. That true?"

Arlene looks at the kid who tattled, and her eyes widen. "Oh, why hello there!" she says to the kid, who recoils away from her. "I didn't even recognize you since you weren't running away wimpering like a baby." She turns back to Henry. "Yep, he tried to steal candy from me and my friend earlier. So, I kicked his butt." She says this all pleasantly and without hesitation. A daring smile appears on her lips. "Do you have something to say about that?"

Henry hesitates for a moment, unsure of how to handle her confident tone. His friend draws his attention and shakes his head in warning. Turning back to you, Henry says, "Hey, no worries. If this dope over here tried to take your candy, he got what was coming to him. But tell ya what, you seem pretty cool. Wanna come with us and egg the schoolhouse?"
"""

block44_2_1 = """

Arlene turns to you and whispers in your ear. "Egging the school *does* sound fun, but that is definitely those kids' candy." She nods towards the candy bags. "We should *probably* bring it back to them. What would you like to do?"
"""

block44_3 = """

While they are being noisy, Arlene turns to you and whispers, "Hey, that's the jerk who stole our candy." She points out a kid hunkered behind his friends, clearly trying to stay out of sight. "Plus, they have those kids' bags." She grins meanly. "Shall we correct this situation?"
"""

block44_4 = """

While they are being noisy, Arlene turns to you and whispers, "Hey, that's the jerk who stole our candy." She points out a kid hunkered behind his friends, clearly trying to stay out of sight, and grins meanly. "Shall we correct this situation?"
"""

block44_5 = """

While they are being noisy, Arlene turns to you and whispers, "Hey, these are the jerks who stole those kids' candy. Look at those bags." She grins meanly. "Shall we correct this situation?"
"""

block44_6 = """ Arlene cringes. "Look," he continues. "I'm just jokin' around. No need to take offense. But hey, you seem pretty cool. How's about you come egg the schoolhouse with us? The name's Henry, by the way."

Arlene grins. The prospect of egging something seems to have lightened her attitude towards these guys. "And I'm Arlene. You can just call my friend here, Reader."

"Reeter?" Henry asks. "That's a pretty odd name. Alright, Arlene. Reeter. How's about egging the school with us?"
"""

block45 = """
You return to the area where the high schoolers were hanging out, but there's no one here. Strange that they wouldn't sit around and wait for a total stranger to come back.
""" |> contentBlock |> label "high-schoolers-gone" |> goto "intersection-1" |> onLeave (setVars [SetBool "high-schoolers-gone" True])

block46 = choiceBlock """
"Well," Arlene says, drawing out the word in a way that suggests she has an ultimatum coming up. "Egging the school *does* sound like fun, but I have one teensy little concern." She pinches her forefinger and thumb together as she says "teensy."

Henry raises an eyebrow. "Oh yeah, and what might that be?"

"Those bags of candy." She nods towards the colorful trick or treat bags. "They don't quite fit your image. They wouldn't happen to be stolen by any chance, would they?"

"Ha! Look at this, boys," Henry says, raising and open palm towards Arlene. "We got a regular detective here." The group chortles, and Henry returns his gaze to Arlene. "As a matter of fact, we nabbed 'em from a couple of brats down the street. What of it?"

"Oh, nothing much. I'm afraid I just wouldn't feel right galavanting with a bunch of candy stealing bullies."

"What did you call us?" Henry growls. Somehow, he doesn't seem to think stealing candy from "a couple brats" counts as being a bully.

"Oh, I apologize if I was unclear," Arlene says. Her smile gives away how much she is enjoying getting under Henry's skin. "I called you bullies. Now, if you would kindly return that candy..."

"SHUT UP!" Henry thunders. "I thought you were cool, but clearly you're both little twerps just like those kids." He looks to his posse. "Let's show 'em not to mess with us, boys!" There is a roar of agreement, and now a group of high school seniors are charging at you and Arlene.

5 v 2. Seems fair.
"""
  [ ("Stay and fight."
    , Just "stay-and-fight"
    , Nothing
    , Nothing
    )
  , ("Run like a wimp."
    , Just "run-from-fight"
    , Nothing
    , Nothing
    )
  ] True |> label "take-back-candy"

block47 = conditionalTextBlock
  [ (.bool, "chased-bully", True, """
Your plan is to take the three on the left and give Henry and the punk you chased down to Arlene, but things don't go quite as you planned. In fact, they go much better. The punk you chased down wimps out and flees as soon he sees you and Arlene aren't afraid of the fight. So, it turns into a 4 v 2.

A fist flies towards your face and you barely dodge to the side, your leg still extended so the bully trips over it and stumbles past you. Then the other one is oncoming. He gets a shot in at your ribs, but entirely fails to protect the spot you were aiming for. This happens to be his face, and he topples to the ground, out cold. Nice hook!

Next to you, Arlene seems to be having no trouble with Henry and the other one. In fact, she is laughing too hard to actually hit them. Henry's face is burning red as he swings at Arlene again. But his buddy goes in for Arlene at the same moment and takes Henry's fist to the back of his head. He stumbles back dazed.

The bully that stumbled behind you grabs you in a chokehold as you are watching Arlene. Maybe you should be paying more attention? But that's okay because Arlene is paying attention. She ducks another of Henry's frenzied punches and shoves him backwards. He slams into a tree and proceeds to start crying. Not so tough now.

Seeing Henry downed, the guy holding onto your neck lets go and runs to get Henry up. It looks like the fight is over.

"I'll be taking these," Arlene says, as she scoops up the candy bags. "Well, wasn't that fun?" she says to you. "Here, hold on to the candy." She hands you the children's candy.

You leave the dazed bullies in the intersection.
  """)
  ]
  """
The odds look pretty bad as five high school seniors charge at you, but these daunting odds don't seem to faze Arlene. She catches the frontmost one in the nose with an oddly powerful left hook. He staggers backwards, both hands on his bleeding nose, and flees the fight almost immediately. What a wimp.

A fist flies towards your face and you barely dodge to the side, your leg still extended so the bully trips over it and stumbles past you. Then the other one is oncoming. He gets a shot in at your ribs, but entirely fails to protect the spot you were aiming for. This happens to be his face, and he topples to the ground, out cold. Nice punch!

Next to you, Arlene seems to be having no trouble with Henry and the other one. In fact, she is laughing too hard to actually hit them. Henry's face is burning red as he swings at Arlene again. But his buddy goes in for Arlene at the same moment and takes Henry's fist to the back of his head. He stumbles back dazed.

The bully that stumbled behind you grabs you in a chokehold as you are watching Arlene. Maybe you should be paying more attention? But that's okay because Arlene is paying attention. She ducks another of Henry's frenzied punches and shoves him backwards. He slams into a tree and proceeds to start crying. Not so tough now.

Seeing Henry downed, the guy holding onto your neck lets go and runs to get Henry up. It looks like the fight is over.

"I'll be taking these," Arlene says, as she scoops up the candy bags. "Well, wasn't that fun?" she says to you. "Here, hold on to the candy." She hands you the children's candy.

You leave the dazed bullies in the intersection.
"""
  |> label "stay-and-fight" |> goto "intersection-1" |> onLeave (setVars [SetBool "has-candy" True, SetBool "high-schoolers-gone" True])

block48 = """
As soon as the going gets rough, you flee the fight. Arlene looks from you to the charging bullies, distressed. After a moment, she turns around and catches up with you. The bullies are too busy laughing and jeering to bother chasing.

"Aww, come on!" Arlene complains, when you stop a safe distance away. "We totally could have taken them."

You remind her that it was five of them versus only you two, but that doesn't change Arlene's opinion.

"Well, I guess we won't be able to get that candy back," Arlene sighs.
""" |> contentBlock |> label "run-from-fight" |> goto "intersection-1" |> onLeave (setVars [SetBool "has-candy" False, SetBool "high-schoolers-gone" True])


block49 = """
"Pardon me," Arlene says, cutting off the high schoolers' laughter. "Sorry to interrupt that raucous noise you're making, but I believe you have some candy that doesn't belong to you."

"Is that so?" Henry asks, threateningly. "And what do you propose we do with that candy, sweetcheeks?"

"Well," Arlene says irritably, "I *was* going to suggest simply returning it, but I am quite tired of listening to your nicknames for me. I think I'd rather punch that ugly mouth they keep spewing out of." Before anyone has a moment to react, Arlene closes in on Henry and catches him in the mouth with an unexpectedly strong left hook. Her fist connects with a wet cracking sound, and Henry staggers back, his mouth bloodied and suddenly down two teeth.

"Ge' her! Ge' her!" he screams through his cupped hand. But none of his friends seem too interested in taking up the fight. They take a couple steps back.

"Boo!" Arlene yells, lunging forward. The high schoolers take off, leaving the candy behind. {{bully-ditches-your-candy}} Henry casts you both one last dirty glance and runs to catch up with his posse.

You pick up the candy that was left behind.
""" |> contentBlock |> label "correct-the-situation" |> goto "intersection-1" |> onLeave (setVars [SetBool "has-candy" True, SetBool "high-schoolers-gone" True])

block50 = choiceBlock """
Despite some concern that the bags of candy don't belong to the high schoolers, you let it go in favor of good, old-fashioned mischief. And, if you wanted to get that candy later, that would be unfortunate because on the way to the school, one of the teens splits up to drop the candy off at his house, promising to meet you at the school.

The remaining group of teens lead you and Arlene down a couple blocks until you arrive at a large, brown building with three rows of regularly spaced windows running along its sides. The parking lot is deserted, and the teens pull out a couple large crates of eggs from a backpack. The other arrives just as the first egg is about to be thrown. He is carrying an entire arsenal of toilet paper and shaving cream.

You and Arlene gladly join these hooligans in attempting to have at least one egg splattered on every window as well as mummifying every tree.

However, in the middle of your fun, the cops show up. Figures.

A pudgy officer steps out of the police car alongside his incredibly fit partner.

Arlene looks at the stain on the pudgy officer's shirt and mutters, "Uh oh..."

"Hey!" the pudgy one shouts. "You kids stop that immediately and don't move!"

Apparently, the group of teens you fell in with aren't the most obedient crowd (I know, I'm surprised too) because instead of not moving, they take off towards the wooded area behind the school. Henry yells, "Follow us! We know a way out of here! Just gotta hop the fence!"

"Not the fence!" growls the pudgy cop. "Damn them. They know my weakness."

"Oh, my. Such a predicament! What shall we do, Reader?" Arlene asks, excitedly.
"""
  [ ("Follow the group."
    , Just "follow-the-group"
    , Just (setVars [add "morality" (-2)])
    , Nothing
    )
  , ("Split up and run with Arlene."
    , Just "split-up"
    , Just (setVars [add "morality" (-2)])
    , Nothing
    )
  , ("Turn yourself in."
    , Just "turn-yourself-in"
    , Just (setVars [add "morality" 15])
    , Nothing
    )
  , ("Assault the cops."
    , Just "assault-the-cops"
    , Just (setVars [add "morality" (-50)])
    , Nothing
    )
  ] True |> label "egg-the-school"

block51 = conditionalTextBlock
  [(.string, "reader-costume", "godzilla", """
You and Arlene follow the teens with the cops chasing close behind, calling for you to stop immediately. The group runs into the woods and weaves this way and that until you all arrive at a tall, mesh fence. You have put some distance between yourselves and the cops, but not much.

The teens climb quickly over the fence and call for you to hurry up. And you would absolutely love to hurry up except you forgot you wore that ridiculous Godzilla costume and have absolutely no hope of getting over the tall fence, even with Arlene trying her best to help you. Realizing you are out of options, you begin stripping the costume off, piece by piece.

Unfortunately, you are a bit too slow and end up standing in front of two angry cops in your underwear. Woops.
  """)]
  """
You and Arlene follow the teens with the cops chasing close behind, calling for you to stop immediately. The group runs into the woods and weaves this way and that until you all arrive at a tall, mesh fence. You have put some distance between yourselves and the cops, but not much.

The teens climb quickly over the fence and call for you to hurry up. You hop over the fence after them, and they lead you a roundabout way back out of the forest. The cops are left far behind you.

"You guys are pretty cool," Henry says, patting you amiably on the back. There is a murmur of agreement among the teens. "Look, we're gonna head out for the night, but look for us next year, alright?"

With that, the group disperses.

Arlene turns to you with a big smile on her face. "So. Worth it."
  """
  |> label "follow-the-group"
  |> \b ->
    { b |
      next <- (\vars -> if Dict.get "reader-costume" vars.string == Just "godzilla" then Label "game-over" else Label "intersection-1"),
      onLeave <- (\vars -> if Dict.get "reader-costume" vars.string == Just "godzilla" then setVars [SetString "game-over-text" "Congratulations! You got arrested. You also stripped in front of Arlene. Awkward.", SetBool "high-schoolers-gone" True] vars else setVars [SetBool "high-schoolers-gone" True] vars)
    }

block52 = """
You and Arlene split up from the group and make a break for it on your own. The cops split up too, the fit one running after the teens and the pudgy one following close behind you. Well, he was close at least. At some point, he must have run out of breath because he is no longer anywhere in sight.

"Oh, my. That man really needs to hit the gym," Arlene giggles.
""" |> contentBlock |> goto "intersection-1" |> label "split-up" |> onLeave (setVars [SetBool "high-schoolers-gone" True])

block53 = """
You raise both your hands into the air and approach the cops, apologizing profusely for your heinous crimes against a window. Arlene watches you get arrested, dumbfounded. Eventually, she just walks away shaking her head. You'll probably see her once you get back to the Party. You know, after your parents pick you up from the police station.
"""
  |> contentBlock
  |> goto "game-over"
  |> label "turn-yourself-in"
  |> onLeave
    (setVars [
      SetBool "high-schoolers-gone" True,
      SetString "game-over-text" "You turned yourself in. Uhh... good job, I guess?"
    ])

block54 = """
You charge at the cops, screaming incoherently and foaming from the mouth. They pull out their guns, but that doesn't stop you. Well, not until they pull the trigger.
"""
  |> contentBlock
  |> goto "game-over"
  |> label "assault-the-cops"
  |> onLeave
    (setVars [
      SetBool "high-schoolers-gone" True,
      SetString "game-over-text" "Congratulations! You got shot in the face.",
      SetString "game-over-text-2" "No, seriously. You assaulted two men just trying to do their jobs. You are an awful person."
    ])

goHome =
  choiceBlock ""
    [ ("Go home. *(This is it. Night's over.)*"
      , Just "go-home-confirmed"
      , Nothing
      , Just (.bool >> Dict.get "has-candy" >> Maybe.withDefault False >> not)
      )
    , ("Go home and keep children's candy. *(This is it. Night's over. And you're a jerk.)*"
      , Just "go-home-confirmed"
      , Just (setVars [add "candy" 967, add "morality" (-7)])
      , Just (.bool >> Dict.get "has-candy" >> Maybe.withDefault False)
      )
    , ("Return to trick or treating."
      , Just "intersection-1"
      , Nothing
      , Just (.bool >> Dict.get "done-with-everything" >> Maybe.withDefault False >> not)
      )
    ] True
    |> label "go-home"
    |> \b ->
      { b |
        contentGenerator <- (\_ vars _ ->
          let locations = ["high-schoolers-gone", "done-with-kids", "done-with-old-lady", "visited-park", "visited-creepy-music-house", "visited-house-with-scarecrow", "visited-down-road"]
              completion = List.length <| List.filter identity <| List.map (flip Dict.get vars.bool >> Maybe.withDefault False) locations
              completionText =
                  if | completion == 0 -> goHome_completion_0
                     | completion < 5 -> goHome_completion_1
                     | completion < 7 -> goHome_completion_2
                     | otherwise -> ""
              returnCandyText = if (Dict.get "has-candy" vars.bool |> Maybe.withDefault False) then goHome_return_candy else ""
          in Markdown.toHtml <| completionText ++ returnCandyText
        )
      }

goHome_completion_0 = """
You tell Arlene that you're ready to head home for the night.

"What?" Arlene exclaims. "We literally *just* got here. A-are you sure you don't want to stay a little longer?" she asks.
"""

goHome_completion_1 = """
You tell Arlene that you're ready to head home for the night.

"You want to go home *already*? B-but there's so much still to do!" she exclaims.
"""

goHome_completion_2 = """
You tell Arlene that you're ready to head home for the night.

"Ahh, I was hoping to do a little more trick or treating, but I suppose we *did* cover a lot of ground," she says. You can hear in her tone that she'd like to stick around a bit longer.
"""

goHome_return_candy = """ "Oh, I nearly forgot," Arlene continues. "You're still holding onto that candy for those kids, right? Shall we return it before heading back?"
"""

goHomeConfirmed =
   { emptyStoryBlock |
      contentGenerator <- (\_ vars _ ->
        let locations = ["high-schoolers-gone", "done-with-kids", "done-with-old-lady", "visited-park", "visited-creepy-music-house", "visited-house-with-scarecrow", "visited-down-road"]
            completion = List.length <| List.filter identity <| List.map (flip Dict.get vars.bool >> Maybe.withDefault False) locations
            stoleCandy = (Dict.get "has-candy" vars.bool |> Maybe.withDefault False)
            goHomeText =
                if | completion < 7 && stoleCandy -> go_home_confirmed_disapointing_stolen_candy
                   | completion < 7 && not stoleCandy -> go_home_confirmed_disapointing
                   | completion >= 7 && stoleCandy -> go_home_confirmed_stolen_candy
                   | otherwise -> go_home_good_ending
        in Markdown.toHtml <| goHomeText
      ),
      onLeave <- (\vars ->
        if (Dict.get "has-candy" vars.bool |> Maybe.withDefault False)
        then setVars [SetString "game-over-text" "Congratulations! You actually stole candy from children. Quite a lot of it, at that. Ever hear the expression \"stealing candy from a baby\"? Yeah, you pretty much did that. Jerk.", SetString "game-over-text-2" "But you're still a candy-stealing meanie."] vars
        else setVars [] vars
      )
    } |> label "go-home-confirmed" |> goto "game-over"

go_home_confirmed_disapointing = """
A portal opens beside Arlene, and she sighs. "Oh, dear. This again... I guess this means the night really is over. I shall see you back at the Party, I suppose!" She steps into the portal and disappears.
"""

go_home_confirmed_stolen_candy = """
"Well, then!" Arlene huffs. "I didn't realize you were such a jerk! Stealing candy from children... of all the things." A portal opens beside her, and she steps into it without looking back. Ouch.
"""

go_home_confirmed_disapointing_stolen_candy = """
A portal opens beside Arlene, and she sighs. "Oh, dear. This again... I guess this means the night really is over... Well, that is quite alright. I should rather end the night early than continue trick or treating with someone who steals candy from children. Hmph." She steps into the portal and disappears.
"""

go_home_good_ending = """
A portal opens beside Arlene, and she sighs. "Oh, dear. This again... I guess this means the night really is over. Well, I had a lot of fun! I think I shall remember this night for quite some time." She moves towards the portal, pokes a hesitant finger into it, then looks back at you. "See you back at the Party!" With that, she disappears into the rift.
"""

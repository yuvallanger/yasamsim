# YasamSim - A Yasam Simulator

Main online repository:

<https://gitlab.com/yuvallanger/yasamsim/>

Mirrors:

<https://bitbucket.org/yuvallanger/yasamsim/>
<https://github.com/yuvallanger/yasamsim/>

# YasamSim development notes

## 2015-08-09

I've read and skimmed quite a few guides today. A [PPDP 2014 Yampa] talk
slides, an [Almost a Netwire 5 Tutorial] and now [Getting Into Netwire].

Confusion will be my epitaph, but it is getting better.

[PPDP 2014 Yampa]: <http://www.cs.nott.ac.uk/~nhn/Talks/PPDP2014-tutorial.pdf>
[Almost a Netwire 5 Tutorial]: <http://todayincode.tumblr.com/post/96914679355/almost-a-netwire-5-tutorial>
[Getting Into Netwire]: <http://phaazon.blogspot.com/2015/03/getting-into-netwire.html>

## 2015-08-04

Reading about FRP in [Functional Reactive Animation].
It is mostly things I do not comprehend. The notation is a bit weird.

## 2015-08-02

Finally wrote the image loading function, hurray!

It is using some really iffy Either a b management, though. I should really
get that rewritten.

TODO:

* [ ] Find a better way to handle errors with Either.

## 2015-07-31

TODO:

* [ ] Integrate acceleration instead of using impulse velocity
* [ ] Add dampening

Thanks Welkin!

Also, there's a [screen capture][yasamsim_2015_07_31] of the current progress:

![YasamSim 2015-07-31][yasamsim_2015_07_31]

The yasamnik sprite was chissled out of pure pixels by [moomoohk].

## 2015-07-30

Our hero can now face in the direction it is walking. Hurray!

### 21:29

A few short term goals:

* Convert the png assets into a gloss Picture.
* Figure out what's the deal with FRP and how to use it here.

[yasamsim_2015_07_31]: </yasamsim-2015-07-31.gif>
[moomoohk]: <https://moomoohk.github.io/>
[Functional Reactive Animation]: <http://conal.net/papers/icfp97/icfp97.pdf>

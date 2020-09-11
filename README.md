# SpeakerSetup

Calculates ideal speaker position and speaker stand dimensions given speaker dimensions, desired placement, and seating location.

Arguments
- height of speaker
- width of speaker
- depth of speaker
- distance from center of tweeter to top of speaker
- distance from center of tweeter to side of speaker (measuring towards middle of soundstage)
- distance from front of tweeter to back wall
- distance between tweeters of two speakers
- distance from ear position to back wall
- distance from ear position to ground
- either
	- desired height of speaker from ground
	- desired tiltback angle for speaker

Produces
- angle from speaker to back wall
- distance from inside bottom corner of speaker to the wall
- distance from outside bottom corner of speaker to the wall
- either
	- if given desired height of front-bottom of speaker to ground
		- tiltback angle for speaker
		- distance from front-bottom of speaker to ground
	- if given desired tiltacbk angle for speaker
		- distance from front-bottom of speaker to ground
		- distance from back-bottom of speaker to ground

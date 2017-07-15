(
SynthDef("simple", { arg dur=1.0,freq=440.0,amp=0.2,pan=0.0;
	var osc;
	osc = EnvGen.kr(Env.triangle(dur,amp), doneAction: 2) * SinOsc.ar(freq);
	Out.ar(0,Pan2.ar(osc,pan));
}).writeDefFile.load(s);
)

(
SynthDef("reverb1", {arg mix=0.2, decaytime=15,in=0, out=0;
	var input;
	var p,e;
	var f1, f2,z,a=4,c=5;
	input = (0.2 * In.ar(in,2));

	x = DelayN.ar(input, 0.048);
	y = Mix.ar(CombL.ar(x, 0.1, LFNoise1.kr(Array.fill(c,{0.1.rand}), 0.04, 0.05), decaytime));
	a.do({ y = AllpassN.ar(y, 0.050, [0.050.rand,0.050.rand], 1) });
	Out.ar(0, (input+(mix*y)));
}).writeDefFile.load(s);
)

(
SynthDef("reverb2", {arg mix=0.2, decaytime=15;
	var input;
	var p,e;
	var f1, f2,z,a=4,c=5;
	input = (0.2 * In.ar(0,2));

	x = DelayN.ar(input, 0.048);
	y = Mix.ar(CombL.ar(x, 0.1, LFNoise1.kr(Array.fill(c,{0.1.rand}), 0.04, 0.05), decaytime));
	a.do({ y = AllpassN.ar(y, 0.050, [0.050.rand,0.050.rand], 1) });
	ReplaceOut.ar(0, (input+(mix*y)));
}).writeDefFile.load(s);
)

(
SynthDef("play-buffer", { arg bufnum, amp=1.0, rate=1.0, looping=1,out=0;
	Out.ar(out, amp * PlayBuf.ar(1,bufnum,rate, loop: looping));
}).writeDefFile.load(s);
)

(
SynthDef("granulate", {arg dur=1, amp=1.0, bufnum=1,rate=1.0, gdur=0.1, gamp=0.1, speed=1.0, out=0;
	var tr,bl,trate=100,pan, env;
	
	env = EnvGen.kr(Env.linen( (dur * 0.1), (dur * 0.5), (dur * 0.4) ,amp), doneAction: 2);
	bl = BufDur.kr(bufnum);
	e = Line.kr(0,bl,(speed * bl));
	pan = WhiteNoise.kr(0.6);
	tr = TGrains.ar(2,Dust2.ar(trate),bufnum,rate,e,gdur,pan,gamp);
	Out.ar(out,tr);
}).writeDefFile.load(s);
)


(
SynthDef("simple-osc", { arg dur=1.0,freq=440.0,amp=0.2,pan=0.0, bufnum;
	var osc;
	osc = EnvGen.kr(Env.triangle(dur,amp), doneAction: 2) * Osc.ar(bufnum, freq);
	Out.ar(0,Pan2.ar(osc,pan));
}).writeDefFile.load(s);
)

(
SynthDef("play-buffer", { arg bufnum, amp=1.0, rate=1.0, looping=1,out=0;
	Out.ar(out, Pan2.ar(amp * PlayBuf.ar(1,bufnum,BufRateScale.kr(bufnum) * rate, loop: looping) , IRand(-1.0, 1.0)));
}).writeDefFile.load(s);
)

(
SynthDef("granulate", {arg dur=1, amp=1.0, bufnum=1,rate=1.0, gdur=0.1, gamp=0.1, speed=1.0, out=0;
	var tr,bl,trate=100,pan, env;
	
	env = EnvGen.kr(Env.linen( (dur * 0.1), (dur * 0.5), (dur * 0.4) ,amp), doneAction: 2);
	bl = BufDur.kr(bufnum);
	e = Line.kr(0,bl,(speed * bl));
	pan = WhiteNoise.kr(0.6);
	tr = TGrains.ar(2,Dust2.ar(trate),bufnum,rate,e,gdur,pan,gamp);
	Out.ar(out,tr);
}).writeDefFile.load(s);
)


(
SynthDef("fm-env", { arg freq,mratio=1,index=1.0,amp=1.0,dur;
	var car,mod,ampf,devf,af,df,ampenv,indexenv;
	ampenv = Env.newClear(8);
	indexenv = Env.newClear(8);
	ampf = Control.names([\ampenv]).kr( ampenv.asArray );
	devf = Control.names([\indexenv]).kr( indexenv.asArray );
	
	af = EnvGen.kr(ampf, timeScale: (dur / 100.0), levelScale: amp, doneAction: 2);
	df = EnvGen.kr(devf, timeScale: (dur / 100.0), levelScale: index);

	mod = SinOsc.ar(freq * mratio);
	car = SinOsc.ar(freq + (df * mod * freq));
	Out.ar(0, [car*af,car*af]);
}).writeDefFile.load(s);
)

(
SynthDef("randomness1", {arg density, id;
		SendTrig.kr(Dust.kr(density),id,PinkNoise.ar(1000.0));
}).writeDefFile.load(s);
)

(
SynthDef("pitch-track",{arg minfreq=60, maxfreq=2000.0, ampthresh=0.02, id=1;
	var in, amp, freq, hasFreq, out;
	in = Mix.new(AudioIn.ar([1,2]));
	# freq, hasFreq = Pitch.kr(in, maxFreq: maxfreq, ampThreshold: ampthresh, median: 2);
	SendTrig.kr(hasFreq, id, freq.cpsmidi.floor.asFloat);
}).writeDefFile.load(s);
)


	

	
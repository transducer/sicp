#lang scheme

; Lecture text obtained from (slight modifications made):
; Eric Grimson, Peter Szolovits, and Trevor Darrell, 6.001 Structure and 
; Interpretation of Computer Programs, Spring 2005. (Massachusetts Institute
; of Technology: MIT OpenCourseWare). http://ocw.mit.edu (accessed 11 12, 
; 2015). License: Creative Commons Attribution - Noncommercial - Share Alike.


; Well, we've been making a simulation. And the simulation is an event-
; driven simulation where the objects in the world are the objects in the 
; computer. And the changes of state that are happening in the world in time are organized to be 
; time in the computer, so that if something happens after something else in the world, then we 
; have it happen after, after the corresponding events happen in the same order in the computer. 
; That's where we have assignments, when we make that alignment. 

; Right now I want to show you a way of organizing time, which is an agenda or priority 
; queue, it's sometimes called. We'll do some we'll do a little bit of just understanding what 
; are the things we need to be able to do to make agendas. And so we're going to have and 
; so right now over here, I'm going to write down a bunch of primitive operations for 
; manipulating agendas. I'm not going to show you the code for them because they're all very 
; simple, and you've got listings of all that anyway. So what do we have? We have things like 
; make-agenda which produces a new agenda. We can ask and we get the current time of an agenda, 
; which gives me a number, a time. We can ask whether an agenda is empty, empty-agenda. 
; And that produces either a true or a false.

; We can add an object to an agenda. Actually, what we add to an agenda is an operation, an 
; action to be done. And that takes a time, the action itself, and the agenda I want to add it 
; to. That inserts it in the appropriate place in the agenda. I can get the first item off an 
; agenda, the first thing I have to do, which is going to give me an action. And I can remove 
; the first item from an agenda. That's what I have to be able to do with agendas. That is a 
; big complicated mess. For an agenda. 

; Agendas

(make-agenda) ; => new agenda
(current-time agenda) ; => time
(empty-agenda? agenda) ; => true/false
(add-to-agenda! time action agenda)
(first-item agenda) ; => action
(remove-first-item agenda)

; Well, let's see how we can organize this thing as a data structure a bit. Well, an agenda is 
; going to be some kind of list. And it's going to be a list that I'm going to have to be able to 
; modify. So we have to talk about modifying of lists, because I'm going to add things to it, 
; and delete things from it, and things like that. It's organized by time. It's probably good to 
; keep it in sorted order. But sometimes there are lots of things that happen at the same time - 
; approximately the same time. 

; What I have to do is say, group things by the time at which they're supposed to happen. So I'm 
; going to make an agenda as a list of segments. And so I'm going to draw you a data structure for 
; an agenda, a perfectly reasonable one. 

;       -----   -----   -----
; -->   | | |   | | |   | | |
;       |||-|-> |||-|-> | |/|
;       -----   -----   -----
;        v       v segment
;    *agenda*  -----          *another segment*
;              | | | 
;              |||||
;              -----
;               v |
;             1O  |
;                 v time
;           ---------------
;           | | | | | | | |
;           | | | | | | | |
;           ---------------

; Here's an agenda. It's a thing that begins with a name. I'm going to do it right now out of 
; list structure. It's got a header. There's a reason for the header. We're going to see the 
; reason soon. And it will have a segment. It will be a list of segments. Supposing this agenda has 
; two segments, they're the car's - successive car's of this list. Each segment is going to 
; have a time - say for example, 10 - that says that the things that happen in this segment are 
; at time 10. 

; And what I'm going to have in here is another data structure which I'm not going to describe, 
; which is a queue of things to do at time 10. It's a queue. And we'll talk about that in a 
; second. But abstractly, the queue is just a list of things to do at a particular time. And I 
; can add things to a queue. This is a queue. There's a time, there's a segment.

; Now, I may have another segment in this agenda. Supposing this is stuff that happens at 
; time 30. It has, of course, another queue of things that are queued up to be done at time 
; 30. Well, there are various things I have to be able to do to an agenda. 

; Supposing I want to add to an agenda another thing to be done at time 10. Well, that's not 
; very hard. I'm going to walk down here, looking for the segment of time 10. It is possible 
; that there is no segment of time 10. We'll cover that case in a second. But if I find a 
; segment of time 10, then if I want to add another thing to be done at time 10, I just increase 
; that queue - "just increase" isn't such an obvious idea. But I increase the things to be done 
; at that time. 

; Now, supposing I want to add something to be done at time 20. There is no segment for time 20. 
; I'm going to have to create a new segment. I want my time 20 segment to exist between time 10 
; and time 30. Well, that takes a little work. I'm going to have to do a CONS. I'm going to have 
; to make a new element of the agenda list - list of segments. I'm  going to have to change. 
; Here's change. 

; I'm going to have to change the CDR of the CDR of the agenda to point that a new CONS of the 
; new segment and the CDR of the CDR of the CDR of the agenda, the CD-D-D-DR. And this is going 
; to have a new segment now of time 20 with its own queue, which now has one element in it. If I 
; wanted to add something at the end, I'm going to have to replace the CDR of this, of this list 
; with something. We're going to have to change that piece of data structure. So I'm going to need 
; new primitives for doing this. But I'm just showing you why I need them. 

; And finally, if I wanted to add a thing to be done at time 5, I'm going to have to change this 
; one, because I'm going to have to add it in over here, which is why I planned ahead and 
; had a header cell, which has a place. If I'm going to change things, I have to  have places 
; for the change. I have to have a place to make the change. 

; If I remove things from the agenda, that's not so hard. Removing them from the beginning 
; is pretty easy, which is the only case I have. I can go looking for the first, the first 
; segment. I see if it has a non-empty queue. If it has a non-empty queue, well, I'm going to 
; delete one element from the queue, like that. If the queue ever becomes empty, then I have to 
; delete the whole segment. And then this, this changes to point to here. So it's quite a 
; complicated data structure manipulation going on, the details of which are not really very 
; exciting. 

; Now, let's talk about queues. They're similar. Because each of these agendas has a queue in 
; it. What's a queue? A queue is going to have the following primitive operations. To make a 
; queue, this gives me a new queue. I'm going to have to be able to insert into a queue a 
; new item. I'm going to have to be able to delete from a queue the first item in the queue. 
; And I want to be able to get the first thing in the queue from some queue. I also have to be 
; able to test whether a queue is empty. 

; Queue
(make-queue) ; => new queue
(insert-queue! queue item)
(delete-queue! queue)
(front-queue queue)
(empty-queue? queue) 

; And when you invent things like this, I want you to be very careful to use the kinds of 
; conventions I use for naming things. Notice that I'm careful to say these change something 
; and that tests it. And presumably, I did the same thing over here. OK, and there should be 
; an empty test over here. 

; OK, well, how would I make a queue? A queue wants to be something I can add to at the 
; end of, and pick up the thing at the beginning of. I should be able to delete from the 
; beginning and add to the end. Well, I'm going to show you a very simple structure for that. 
; We can make this out of CONSes as well. 

;    ----| 
;        v
;       -----
;       | | |
;       -----
;  front | | rear
;        v -----v    *adding is extending between the rear or front*
;    -----    -----
;    | | | -> | | |
;    -----    -----
;     |        |
;     v        v
;     1        2

; Here's a queue. It has a queue header, which contains two parts - a front pointer 
; and a rear pointer. And here I have a queue with two items in it. The first item, I don't 
; know, it's perhaps a 1. And the second item, I don't know, let's give it a 2. The reason why 
; I want two pointers in here, a front pointer and a rear pointer, is so I can add to the end 
; without having to chase down from the beginning. 

; So for example, if I wanted to add one more item to this queue, if I want to add on another 
; item to be worried about later, all I have to do is make a CONS, which contains that item, 
; say a 3. That's for inserting 3 into the queue. Then I have to change this pointer here to 
; here. And I have to change this one to point to the new rear. 

; If I wish to take the first element of the queue, the first item, I just go chasing down the 
; front pointer until I find the first one and pick it up. If I wish to delete the first item from 
; the queue, delete-queue, all I do is move the front pointer along this way. The new front of the 
; queue is now this. So queues are very simple too. 

(set-car! <pair> <value>)
(set-cdr! <pair> <value>)

; So what you see now is that I need a certain number of new primitive operations. And I'm 
; going to give them some names. And then we're going to look into how they work, and how 
; they're used. We have set the CAR of some pair, or a thing produced by CONSing, to a new 
; value. And set the CDR of a pair to a new value. And then we're going to look into how they 
; work. 

; I needed setting CAR over here to delete the first element of the queue. This
; is the CAR, and I had to set it. I had to be able to set the CDR to be able to move the rear 
; pointer, or to be able to increment the queue here. All of the operations I did were made out of 
; those that I just showed you on the last blackboard. Good. Let's pause the time, and take a 
; little break then. 

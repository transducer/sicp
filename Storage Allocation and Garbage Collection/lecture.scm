; Lecture PART 1

; We are getting rid of the last bit of mystery:
; We have blindly doing CARs, CDRs and CONSes assuming there is always another one.

; Procedures needs environment structures which were made out of CONSes.

; The glue that data structures are made of. What is it?

; We have seen a machine, a controller and registers, and maybe a stack. We have said nothing
; about memory. And we will worry about that now. It is an implementational thing.

; Godel: I am going to assign a number to every algebraic expression.
; Our world: if objects are represented by numbers then (cons x y) => 2^x * 3^y
; Then we could extract the parts: (car x) => # of factors of 2 in x 
;                                  (cdr x) => # of factors of 3 in y

; Good scheme except that the numbers rapidly gets much larger than the number of atoms
; in the universe. Only a theoretical usage of this scheme.

; There are other ways: think in little boxes. CONS: | | |. And we arrange them in trees.

; Memory is sort of a big pile of pigeonholes, each which can hold a fixed size object.
; Indexed by an address. Distinct. We can stuff something in these pigeonholes.

; How are we going to impose on this type of linear structure the tree structure?

; It is not hard, there are numerous schemes involved in this.

; One pigeonhole is big enough to hold the address of another and hold information of a
; tag (to describe what is there).

; Classic scheme of representing Lisp structures in a linear memory:
; Divide the memory into two parts: array holding the CARs and array holding the CDRs.
; Sequential or not is not important. That are implementation details.
; Stored in each pigeonhole is a typed object. (e.g., Pair p, Empty e, Number n)

; Assign indeces to the box-and-pointers.

(assign a (car (fetch b)))
; ===>
(assign a (vector-ref (fetch the-cars)
                      (fetch b)))

(assign a (cdr (fetch b)))
; ===>
(assign a (vector-ref (fetch the-cdrs)
                      (fetch b)))

(perform (set-car! (fetch a) (fetch b)))
; ===>
(perform (vector-set! (fetch the-cars)
                      (fetch a)
                      (fetch b)))

(perform (set-cdr! (fetch a) (fetch b)))
; ===>
(perform (vector-set! (fetch the-cdrs)
                      (fetch a)
                      (fetch b)))

; Not too hard to build.

; How are we going to do ALLOCATION?

; Ever so often I say: "I want a CONS." Conses don't grow on trees.

; I have to have some idea of getting the next one. Again many schemes.

; Freelist allocation scheme: all free memory in the world is in a linked list, and
; whenever you need a free cell you grab the first one, make the free list be the cdr of it,
; and then allocate that.

; With freelist method allocation:

(assign a (cons (fetch b) (fetch c)))
; ===>

(assign a (fetch free))
(assign free
        (vector-ref (fetch the-cdrs)
                    (fetch free)))
(perform (vector-set! (fetch the-cars)
                      (fetch a)
                      (fetch b)))
(perform (vector-set! (fetch the-cdrs)
                      (fetch a)
                      (fetch c)))

; There should be some sort of bits being sets for the types. Inessential detail.

; Next problem: we do not have an infinitely large memory.

; If I do this for a little while - suppose I have a million cons memory and it takes a
; microsecond to do one then I'm only going to run out of memory in a second, and that's
; pretty bad. What we do about that ecological disaster I talk about next.


; Lecture PART 2

; Semiconductor company memories are finite. Might not always be that case.

; If it still took a microsecond to do a cons. Well, first of all everyone should now that
; there are about pi * 10^7 seconds in a year. So that is around 10^14 conses in the life of
; a machine. If there were 10^14 words of memory ony our machine you'd never run out. And
; that's not completely unreasonable. 10^14 is not a very large number.

; There is 10^18 cm between us and the nearest star.

; In the current state of affairs 10^14 words of memory is too expensive so we have to do
; with much smaller memories. In general we want to have the illusion of INFINITY. So we
; want to arrange it in such a way that whenever we look the thing is there.

; That's really an important idea.

; We make lots of stuff we don't need. We can recycle them. When we built an environment
; structure for example we built it in a frame, that frame does not have a very long life
; time (meaning its usefulness). It may exist only over the invocation of the procedure.
; Or if it returns to an outer procedure then the life time of the procedure still is
; only the lifetime of the procedure which was exported. Ultimately a lot of that is
; GARBAGE.

; There are other ways to produce garbage as well. Users produce garbage.

; A source of garbage

(define (rev-loop x y)
  (if (null? x)
      y
      (rev-loop (cdr x)
                (cons (car x) y))))

(define (append u v)
  (rev-loop (rev-loop u '()) v))

; The reversal of the first list is never used again after it is copied. It is an
; intermediate result. It is going to be hard to see how anyone will ever access it. In fact
; it will go away.

; We should be allowed to make so much garbage, but there should be a way to reclaim that
; garbage.

; Now comes a very clever technique whereby a Lisp system can prove a small theorem ever
; so often on the form of the following piece of junk will never be accessed again: it can
; have no affect on the future of computation.

; Based on a simple idea. We have data paths and registers in our computer, a stack,
; finite state machine controller and structured memory. Data structures hooked together
; in funny ways. Ultimately things that are in the registers are pointers off to the
; data structures that live in the Lisp structure memory.

; Now, the truth of the matter is that the entire consciousness of this machine is in
; these registers. There is no possible way that the machine, if done correctly, can access
; anything in this Lisp structure memory unless the thing in the Lisp structure memory
; is connected by a sequence of data structures to the registers.

; We cannot get to memory in another way.

; This leads to: if I start with all the lead pointers which are in these registers and
; recursively chase out, marking all the places I can get to by selectors, then eventually
; I mark everything that can be gotten to. Everything that is not marked is garbage and can
; be recycled. 

; We grow new trees of the garbage.

; Next scan through all the memory looking for things that are not marked (every time I come
; across a marked thing I unmark it and every time I come across an unmarked thing I am
; going to link it together in my free list.

; A classic very simple algorithm.

gc
(assign thing (fetch root))
(assign continue sweep)
mark
(branch (not-pair? (fetch thing))
        done)
pair
(assign mark-flag
        (vector-ref (fetch the-marks)
                    (fetch thing)))
(branch (= (fetch mark-flag) 1)
        done)
(perform
 (vector-set! (fetch the-marks)
              (fetch thing)))

mcar
(push thing)
(push continue)
(assign continue mcdr)
(assign thing
        (vector-ref (fetch the-cars)
                    (fetch thing)))
(goto mark)

mcdr
(pop continue)
(pop thing)
(assign thing
        (vector-ref (fetch the -cdrs)
                    (fetch thing)))
(goto mark)
done
(goto (fetch continue))

; That's the entire mark phase.

; The old DEC PDP-6 computer had such a mark-sweep garbage collecters as above. 
              
; Lisp systems require a stack for marking.

; Peter Deutsch and Herb Schorr and Waite from IBM made an algorithm without auxiliary
; memory, by remembering as you walk the data structures where you came from by reversing
; the pointers as you go down and crawling up the reverse pointers as you go up. 
; (A rather tricky algorithm and the first three times you write it it will have a terrible
; bug in it. It is also rather slow [six times as long as amount of memory references]
; because it is complicated.)

; Once the marking phase is completed we can do the sweep phase. Scan every cell in the
; memory and link them into the free-list if they are free and otherwise unmark them.

sweep
(assign free '())
(assign scan (-1+ (fetch memtop)))
slp
(branch (negative? (fetch scan))
        end)
(assign mark-flag
        (vector-ref (fetch the-marks)
                    (fetch scan)))
(branch (= (fetch mark-flag 1)
           unmk))
unmk
(perform
 (vector-set! (fetch the-marks)
              (fetch scan)
              0))
(assign scan( -1+ (fetch scan)))
(goto slp)
end

; There are some SERIOUS disadvantages of mark-sweep algorithms of this sort.

; Address spaces get larger and larger willing to represent more and more stuff, and then
; it gets very costly to scan all of memory. You would really like to only scan the useful
; stuff. If could even be better if you realized that some stuff was known to be good and
; useful and you don't have to look at it more than once or twice. Or very rarely. Whereas
; other stuff you are not so sure about you can look at more detailed during garbage
; collection.

; Here's one that only looks at the part of memory that is known to be useful. This happens
; to be the fastest garbage collector algorithm.

; The Minsky-Feinchel-Yochelson garbage collector algorithm. Invented in 1960-1961 for the
; RLE PDP-1 Lisp, which had 4096 words of list memory, and a drum.

; The whole idea was to garbage collect this terrible memory.

; What Minsky reliazed was that the easiest way to do this is to scan the memory in the same
; sense: walking the good structure, copying it out into the drum, compacted. And then when
; we were done copying it all out, then you swap that back into your memory.

; This algorithm basically depends upon having about twice at much address space as you're
; actually using. In the front space you have a mixture of useful data and garbage.
; Another place is the tospace (which is hopefully big enough), is where we are copying to.
; What happens is that there is a root point you start from and you copy the first thing
; you see (the first thing the root points at), to the beginning of tospace. The first thing
; being a pair or a data structure. You then also leave behind a broken heart saying: I
; moved this object from here to here, giving the place where it moved to. (It is called a
; broken heart because a friend of Sussman who implemented it was a very romantic character.)
; Then you have a new free pointer and you start scanning the data structure you just copied.
; And every time you encounter a pointer you treat it as if it was the root pointer. If there
; is a broken heart there and it's something you have copied you have just replaced the
; pointer with the thing the broken heart points at. If not, you copy it to the next place
; and move the free pointer and leave a broken heart behind and scan. When the scan pointer
; hits the free pointer everything in memory has been copied. And then there is a whole
; bunch of open memory in tospace which you could either make into a free list, if that's
; what you want to do. But generally you don't and sequentially allocate your memory.

; A very very nice algorithm and the one we use in the Scheme we are using.

; No one has found a faster algorithm than that. You can make simple modifications to this
; algorithm which allows it to run in real time. Meaning you don't have to stop to
; garbage collect but you can interleave the consing the machine does when it's running
; with steps of the garbage collection process, so that the garbage collection is
; distributed and the machine doesn't have to stop, and garbage collecting can start.

; In the case of machines with virtual memory this can become a very expensive process.

; Now the mystery to this is sort of gone. Questions?

; Q: I saw the garbage collection running upstairs, and it seemed extremely fast... Did it
;    sweep through all of memory?
; A: No. It swept through exactly what was needed to copy the useful structure. It's a
;    a copying collector and it is very fast. To copy 3MB or something is less than a second
;    in real time.
;    Garbage collectors have to be small. Not because they have to be fast, but because
;    no one can debug a complicated garbage collector. A garbage collector that does not
;    work will trash your memory in such a way that you cannot figure out what the hell
;    happened. You need an audit trail. Because it rearranges everything. You have to prove
;    to yourself that it works because there is no way to debug it. It has to be small
;    enough to hold it in your head. Generally small programs are fast.


; Lecture PART 3

; Not everything can be computed.

; So far we have been dazzled with things that we can compute. But are there things we
; can't compute? We will end the course with things that we'd like to be able to compute,
; but we can't.

; For example if you're writing a compiler you would like a program that would check that
; the thing you're going to do will work. Wouldn't that be nice? You'd like something
; that would catch infinite loops, for example, in programs that were written by users.
; But in general you can't write such a program that will read any program and determine
; if it is an infinite loop.

s[p, a] =          { true  if (p a) converges to value without error
  |  -> argument     false if (p a) loops forever or makes an error
 procedure

; Can you write a procedure that can do this? Assume we have a procedure called safe? that
; computes the value of s. I can show you we cannot do this in several ways:

(define diag1
  (lambda (p)
    (if (safe? p p)
        (inf)
        3)))

(define inf
  (lambda ()
    ((lambda (x) (x x))
     (lambda (x) (x x)))))

(diag1 diag1)

; If it is safe to calculate diag1 of diag1 it should return 3, but if it is safe I procude
; an infinite loop. Therefore by contradiction you cannot produce safe?.

; For those that are boggled I am going to say it in a different way:

; (These are called diag because of Cantor's diagonal argument. These are instances of a
; famous argument which was originally used by Cantor in the late part of the 19th century
; to prove that the real numbers were not countable, that there are too many real numbers to
; be counted by integers. That there are more points on a line, for example, than thera are
; counting numbers. It may or may not be obvious, and I don't want to get into that now.

(define diag2
  (lambda (p)
    (if (safe? p p)
        (other-than (p p))
        false)))

(define other-than
  (lambda (x)
    (if (eq? x 'a)
        'b
        'a)))
    
; safe? only does something dangerous, like calling p of p, if it's safe to do so.  
; So if safe? is defined at all, then this procedure diag2 is always defined and therefore
; safe on any input. So diag2 to diag2 must reduce to other than diag2 of diag2. And that
; is a contradiction and therefore you cannot define safe?.

; I wanted to do this twice so you won't think that it is a trick. They may be both tricks,
; but at least they are slightly different. I just proved what we call the halting theorem,
; and I suppose with that we're going to halt. Hope you have a good time. Quesions?

; Q: What is s?
; A: We cannot say. No one can compute. I am just a machine. Maybe in one case, but not all.
;    If there's a certain number of answers possible and a certain number of inputs possible
;    then the numbers of answers raised to the number of inputs is the number of possible
;    functions on one variable. That is always bigger than the thing you're raising to, the
;    expontent. The number of functions is larger than the number of programs that one can
;    write, by an infinity counting argument. And it's much larger. So there must be a lot
;    of functions that can't be computed by programs.

; Q: Do you see any step between specification and solutions?
; A: Steps between? You're saying how do you go about constructing devices given that we have
;    specifications for the device? 
; Q: Indeed, from specification we go through many layers of design before we come to
;    implementation. Is that realistic?
; A: Well I think that some of it's realistic and some of it isn't. (...) The problem with
;    most software engineering art is that there's no mechanism, other than peephole
;    optimization and compilers, for getting rid of the redundant parts that are constructed
;    when doing top down design. It's even worse: there are very large important structures
;    that you can't construct at all in this way. [Since from far away small details with
;    multiple functions are not seen.] I think the standard top down design is a rather
;    shallow business.

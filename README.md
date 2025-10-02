# Advanced Programming 2025

<img align="right" width="300" src="https://github.com/diku-dk/ap-e2024-pub/assets/55833/2a499f62-386c-4fbb-a66e-108a027364a0">

Welcome to the course website for Advanced Programming (AP) 2025! All material
and general information will be provided here. Announcements, assignment handin,
and the discussion forum remains on Absalon. While this website is a Git
repository, you are not required or expected to use Git to interact with it, but
feel free to do so if convenient for you.

The programming language we will be using is **Haskell**.

The teaching material for each week can be found in the `weekI` subdirectories.

The assignments for each week can be found in the `aI` subdirectories.

## Discord

We have created a Discord server for discussing the course: [Invite
link](https://discord.gg/DBzmDyxt82). Please use your government name rather
than your sweet hacker nick.

You are not *required* or *expected* to use Discord. We continue to monitor the
Absalon discussion forum, and course announcements are posted solely on Absalon.

**Do not use Discord for any kind of administrative matters, e.g., dispensation
requests. These must go through email.**

## Preparation

AP has a reputation as a difficult course, particularly for inexperienced
programmers, and *especially* if you do not have prior experience with
functional programming. If you worry that your background is insufficient, we
recommend spending a little time (re-)familiarising yourself with functional
programming. The specific language and book does not matter, but since we will
use Haskell in AP, you might as well read [Programming in
Haskell](https://www.cs.nott.ac.uk/~pszgmh/pih.html) by Graham Hutton. If you do
not like the style of this book, there are plenty others available (such as
[Haskell from First Principles](https://haskellbook.com/)).

Many Haskell books focus on industrial uses, and go into great detail about how
to use complex libraries and such. This is well and good for real-world
programming, but unnecessary for AP, where we take a more narrow focus. You do
not need to read a thick book, unless you want to.

If you have previously taken the course prior to 2024, see [these notes on
translating Erlang concepts to Haskell](erlang.md).

## Textbooks

We do not mandate a specific textbooks for learning Haskell, but we do have
suggestions. We will also specify some research papers and course notes as
mandated or suggested reading.

* [Programming in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pih.html) (video
  lectures
  [here](https://www.youtube.com/playlist?list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3)
  and
  [here](https://www.youtube.com/playlist?list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc))
* [AP Course Notes](https://diku-dk.github.io/ap-notes/)
* [Course notes from Brent Yorgey's UPenn Haskell
  course](https://www.cis.upenn.edu/~cis1940/spring13/lectures.html)

We will also occasionally suggest reading material labeled *Going Beyond*. This
is material that goes beyond the requirements of the course, but may be of
interest to students who wish to apply the techniques we investigate in applied
settings. That is, students who wish to become capable of writing advanced
Haskell programs that solve real problems.

## Course-specific material

* [Guide for setting up a Haskell development environment](haskell.md)
* [Haskell programming hints](haskell-hints.md)

## Course structure

The course consists of two lectures and two exercise classes every week.

The lectures are on Tuesday 10:00-12:00 in Store UP1 at DIKU and Thursday
13:00-15:00 also in Store UP1 at DIKU.

The exercise classes are both on Thursday, at 10:00-12:00 and 15:00-17:00
respectively.

September 2nd marks the beginning of the course, with the first lecture.

|       | Monday | Tuesday | Wednesday | Thursday | Friday |
|-------|--------|---------|-----------|-----------|--------|
| 10-12 | | Lecture | | Exercises | |
| 13-15 | | | | Lecture | |
| 15-17 | | | | Exercises | |

There are no lectures in week 42, on account of the fall holiday.

### Exercise class locations

Ignore the classroom and "hold" assignments on the official KU schedule.
Exercise classes are held in the following rooms. You are free to go where you
wish, but try to distribute the load evenly. Each room will have a TA to help
with exercises and assignments.

#### 10:00-12:00

- NBB 1.01.B.080

- NBB 1.01.B.082

- NBB 1.01.F.64

- NBB 1.01.F.70

The following room is also booked, and will be staffed with a teacher in at
least the first week (perhaps more if necessary).

- NBB 1.1.E.015

#### 15:00-17:00

- HCØ Aud 04

- HCØ Aud 05

- HCØ Aud 06

- HCØ Aud 10

## Assignments

There are 6 assignment in total during the course with deadlines every week.
They overlap slightly to allow for more flexibility in your scheduling, but
think of them as weekly assignments.

The assignments will be graded with points from 0 to 4 and it is not possible to
re-hand-in any of the assignments.

Assignments are made to be solved in groups of up to three students. We strongly
encourage you not to work alone. Each group must make their own solutions and
cannot share implementations and report with other. You may discuss material and
ideas.

### Handin

Assignment handins are on Absalon. You *must* join an Assignment Group, even
when handing in alone. This is done on Absalon by going to *People*, then
*Groups* and picking either an empty one, or one where the existing members have
agreed to let you join. When one member of an Assignment Group hands in the
assignment, it counts as a handin for the entire group.

### General assignment rules

The following rules apply to all assignments. They are intended to ease our
correction process, and in particular to allow automated testing. Consider the
assignments to be a specification or API that you are asked to implement.

1. Do not modify the types of any exported definitions in the handout, except
   when the assignment text explicitly instructs you to do so.

2. Do not rename or remove any exported definitions that are present in the
   handout, except when the assignment text explicitly instructs you to do so.

3. Do not remove anything from module export lists.

4. Do not rename modules or otherwise modify the file tree. (You may add new
   files if you wish, although it is rarely necessary.)

5. Your code should compile without warnings. (Do not achieve this by disabling
   warnings.)

6. When handing in, you must hand in a complete workable program (including
   unmodified files from the handout).

7. When handing in, do not include temporary build files (such as
   `dist-newstyle`), editor backup files, or various other computer detritus.
   Run e.g. the `tree` command and read the file listing and ponder for each
   file whether it is something it makes sense to hand to your TA. At a
   *minimum*, run `cabal clean` before handing in.

8. You are allowed to use any Haskell module that is available in the packages
   specified in the `.cabal` file in the handout. You are not allowed to add
   additional package dependencies.

9. Your zip file, should contain a single top-level folder with an appropriate
   name (e.g. `handin`).

Violation of these rules will result in points deductions. If you violate these
rules at the exam, it will negatively influence your grade.

## Exam

The exam will be a multi-day take-home exam held in one of the exam weeks. It
will strongly resemble the mandatory assignments in content and form, with the
following differences:

* It is strictly individual.

* It is roughly the size of three assignments (the exam is allotted a workload
  of 25 hours in the course description).

* You receive only summative feedback (i.e., a grade).

[Examples of previous exams can be found here.](exams/)

### Qualification

To qualify for the exam you are required to achieve at least 50% of the total
number of points in the assignments (that is, 12 points at minimum). You also
need to get *at least* one point in each of the assignments.

### If you qualified in a previous year

If you qualified for the exam in a previous year, then you are still qualified.
When we approach the exam, and we have to send in a list of students who have
qualified, we will post a message telling you to contact us to inform us of
this.

We still recommend you follow the course.

### Date

Week 44, *most likely* the entire week (this is currently unclear).

You are not expected to work full time for the entire period. The intended exam
workload is specified as 25 hours of work in the course description.

## Reexam

The reexam takes the same form as the ordinary exam (with the possibility of
switching to an oral exam if there are very few attendees, but this is highly
unlikely) and is held in week 5.

If you did not qualify for the ordinary exam, qualification for the re-exam can
be achieved by (re)submission and approval of the mandatory assignments. The
assignments must be submitted on Absalon no later than three weeks before the
re-exam date.

## AI Policy

You are allowed to use AI-generated code and text in assignments and the exam in
AP, but you must *explicitly* indicate which parts have been generated this way,
and which tool you have used. If you do not cite properly, then you are
conducting academic dishonesty (i.e., plagiarism), which is treated very
seriously by the university.

Note also that it is my experience that AI tools will tend to generate "best
practice" Haskell, which is *not* what you are expected to write in AP. To
demonstrate that you have mastered the course learning goals, you must implement
your code using the techniques emphasized by the course, even in those cases
where a simpler solution may be possible (and preferred by an AI assistant).
This is because we are teaching big hammers, but the constraints of an exam do
not leave space for big nails.

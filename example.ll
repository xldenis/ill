; ModuleID = 'example'
source_filename = "<string>"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.13.0"

%Double = type { i64, double }
%Int = type { i64, i64 }
%Monad = type { i64 }
%Applicative = type { i64 }
%Functor = type { i64 }
%Bounded = type { i64 }
%Enum = type { i64 }
%Foldable = type { i64 }
%Ord = type { i64 }
%Eq = type { i64 }
%Group = type { i64 }
%Monoid = type { i64 }
%Semigroup = type { i64 }
%Show = type { i64 }
%Bool = type { i64 }
%Either = type { i64 }
%List = type { i64 }
%Maybe = type { i64 }
%Ordering = type { i64 }
%Tuple = type { i64 }
%Unit = type { i64 }
%String = type { i64, i8* }

@.str = private unnamed_addr constant [5 x i8] c"%lld\00", align 1
@.str.1 = private unnamed_addr constant [5 x i8] c"%s%s\00", align 1
@.str.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #0

; Function Attrs: nounwind
define noalias %Double* @mkDouble(double %d) local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 16)
  %1 = bitcast i8* %0 to %Double*
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %2 = bitcast i8* %.repack4 to double*
  store double %d, double* %2, align 8
  ret %Double* %1
}

; Function Attrs: nounwind
define noalias %Int* @mkInt(i64 %d) local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 16)
  %1 = bitcast i8* %0 to %Int*
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %2 = bitcast i8* %.repack4 to i64*
  store i64 %d, i64* %2, align 8
  ret %Int* %1
}

define i8* @apply1({ i8*, i8, [1 x i8*] }* %closure, i8* %argPtr) local_unnamed_addr {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 1
  %1 = load i8, i8* %0, align 8
  %2 = icmp eq i8 %1, 1
  br i1 %2, label %"Arity 1", label %default

"Arity 1":                                        ; preds = %entry
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  store i8* %argPtr, i8** %3, align 8
  %4 = bitcast { i8*, i8, [1 x i8*] }* %closure to i8* ({ i8*, i8, [1 x i8*] }*)**
  %5 = load i8* ({ i8*, i8, [1 x i8*] }*)*, i8* ({ i8*, i8, [1 x i8*] }*)** %4, align 8
  %6 = tail call i8* %5({ i8*, i8, [1 x i8*] }* nonnull %closure)
  ret i8* %6

default:                                          ; preds = %entry
  %7 = add i8 %1, -1
  %8 = sext i8 %7 to i64
  %9 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 %8
  store i8* %argPtr, i8** %9, align 8
  store i8 %7, i8* %0, align 8
  %10 = bitcast { i8*, i8, [1 x i8*] }* %closure to i8*
  ret i8* %10
}

; Function Attrs: nounwind
define noalias %Monad* @MkMonad(%Applicative* %a, { i8*, i8, [1 x i8*] }* %a1) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 24)
  %.repack6 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack6 to %Applicative**
  store %Applicative* %a, %Applicative** %1, align 8
  %.repack8 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack8 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }** %2, align 8
  %3 = bitcast i8* %0 to %Monad*
  ret %Monad* %3
}

; Function Attrs: nounwind
define noalias %Applicative* @MkApplicative(%Functor* %a, { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }* %a2) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 32)
  %.repack8 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack8 to %Functor**
  store %Functor* %a, %Functor** %1, align 8
  %.repack10 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack10 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }** %2, align 8
  %.repack12 = getelementptr inbounds i8, i8* %0, i64 24
  %3 = bitcast i8* %.repack12 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a2, { i8*, i8, [1 x i8*] }** %3, align 8
  %4 = bitcast i8* %0 to %Applicative*
  ret %Applicative* %4
}

; Function Attrs: nounwind
define noalias %Bounded* @MkBounded(i8* %a, i8* %a1) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 24)
  %.repack6 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack6 to i8**
  store i8* %a, i8** %1, align 8
  %.repack8 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack8 to i8**
  store i8* %a1, i8** %2, align 8
  %3 = bitcast i8* %0 to %Bounded*
  ret %Bounded* %3
}

; Function Attrs: nounwind
define noalias %Enum* @MkEnum({ i8*, i8, [1 x i8*] }* %a, { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }* %a2, { i8*, i8, [1 x i8*] }* %a3) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 40)
  %.repack10 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack10 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a, { i8*, i8, [1 x i8*] }** %1, align 8
  %.repack12 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack12 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }** %2, align 8
  %.repack14 = getelementptr inbounds i8, i8* %0, i64 24
  %3 = bitcast i8* %.repack14 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a2, { i8*, i8, [1 x i8*] }** %3, align 8
  %.repack16 = getelementptr inbounds i8, i8* %0, i64 32
  %4 = bitcast i8* %.repack16 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a3, { i8*, i8, [1 x i8*] }** %4, align 8
  %5 = bitcast i8* %0 to %Enum*
  ret %Enum* %5
}

; Function Attrs: nounwind
define noalias %Foldable* @MkFoldable({ i8*, i8, [1 x i8*] }* %a, { i8*, i8, [1 x i8*] }* %a1) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 24)
  %.repack6 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack6 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a, { i8*, i8, [1 x i8*] }** %1, align 8
  %.repack8 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack8 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }** %2, align 8
  %3 = bitcast i8* %0 to %Foldable*
  ret %Foldable* %3
}

; Function Attrs: nounwind
define noalias %Ord* @MkOrd(%Eq* %a, { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }* %a2, { i8*, i8, [1 x i8*] }* %a3, { i8*, i8, [1 x i8*] }* %a4, { i8*, i8, [1 x i8*] }* %a5, { i8*, i8, [1 x i8*] }* %a6, { i8*, i8, [1 x i8*] }* %a7) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 72)
  %.repack18 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack18 to %Eq**
  store %Eq* %a, %Eq** %1, align 8
  %.repack20 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack20 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }** %2, align 8
  %.repack22 = getelementptr inbounds i8, i8* %0, i64 24
  %3 = bitcast i8* %.repack22 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a2, { i8*, i8, [1 x i8*] }** %3, align 8
  %.repack24 = getelementptr inbounds i8, i8* %0, i64 32
  %4 = bitcast i8* %.repack24 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a3, { i8*, i8, [1 x i8*] }** %4, align 8
  %.repack26 = getelementptr inbounds i8, i8* %0, i64 40
  %5 = bitcast i8* %.repack26 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a4, { i8*, i8, [1 x i8*] }** %5, align 8
  %.repack28 = getelementptr inbounds i8, i8* %0, i64 48
  %6 = bitcast i8* %.repack28 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a5, { i8*, i8, [1 x i8*] }** %6, align 8
  %.repack30 = getelementptr inbounds i8, i8* %0, i64 56
  %7 = bitcast i8* %.repack30 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a6, { i8*, i8, [1 x i8*] }** %7, align 8
  %.repack32 = getelementptr inbounds i8, i8* %0, i64 64
  %8 = bitcast i8* %.repack32 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a7, { i8*, i8, [1 x i8*] }** %8, align 8
  %9 = bitcast i8* %0 to %Ord*
  ret %Ord* %9
}

; Function Attrs: nounwind
define noalias %Group* @MkGroup(%Monoid* %a, { i8*, i8, [1 x i8*] }* %a1) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 24)
  %.repack6 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack6 to %Monoid**
  store %Monoid* %a, %Monoid** %1, align 8
  %.repack8 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack8 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a1, { i8*, i8, [1 x i8*] }** %2, align 8
  %3 = bitcast i8* %0 to %Group*
  ret %Group* %3
}

; Function Attrs: nounwind
define noalias %Monoid* @MkMonoid(%Semigroup* %a, i8* %a1) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 24)
  %.repack6 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack6 to %Semigroup**
  store %Semigroup* %a, %Semigroup** %1, align 8
  %.repack8 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack8 to i8**
  store i8* %a1, i8** %2, align 8
  %3 = bitcast i8* %0 to %Monoid*
  ret %Monoid* %3
}

; Function Attrs: nounwind
define noalias %Semigroup* @MkSemigroup({ i8*, i8, [1 x i8*] }* %a) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 16)
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack4 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a, { i8*, i8, [1 x i8*] }** %1, align 8
  %2 = bitcast i8* %0 to %Semigroup*
  ret %Semigroup* %2
}

; Function Attrs: nounwind
define noalias %Functor* @MkFunctor({ i8*, i8, [1 x i8*] }* %a) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 16)
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack4 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a, { i8*, i8, [1 x i8*] }** %1, align 8
  %2 = bitcast i8* %0 to %Functor*
  ret %Functor* %2
}

; Function Attrs: nounwind
define noalias %Show* @MkShow({ i8*, i8, [1 x i8*] }* %a) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 16)
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack4 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a, { i8*, i8, [1 x i8*] }** %1, align 8
  %2 = bitcast i8* %0 to %Show*
  ret %Show* %2
}

; Function Attrs: nounwind
define noalias %Eq* @MkEq({ i8*, i8, [1 x i8*] }* %a) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 16)
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack4 to { i8*, i8, [1 x i8*] }**
  store { i8*, i8, [1 x i8*] }* %a, { i8*, i8, [1 x i8*] }** %1, align 8
  %2 = bitcast i8* %0 to %Eq*
  ret %Eq* %2
}

; Function Attrs: nounwind
define noalias %Bool* @True() local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 8)
  %1 = bitcast i8* %0 to %Bool*
  ret %Bool* %1
}

; Function Attrs: nounwind
define noalias %Bool* @False() local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 8)
  %1 = bitcast i8* %0 to %Bool*
  ret %Bool* %1
}

; Function Attrs: nounwind
define noalias %Either* @Left(i8* %a) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 16)
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack4 to i8**
  store i8* %a, i8** %1, align 8
  %2 = bitcast i8* %0 to %Either*
  ret %Either* %2
}

; Function Attrs: nounwind
define noalias %Either* @Right(i8* %a) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 16)
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack4 to i8**
  store i8* %a, i8** %1, align 8
  %2 = bitcast i8* %0 to %Either*
  ret %Either* %2
}

; Function Attrs: nounwind
define noalias %List* @Cons(i8* %a, %List* %a1) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 24)
  %.repack6 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack6 to i8**
  store i8* %a, i8** %1, align 8
  %.repack8 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack8 to %List**
  store %List* %a1, %List** %2, align 8
  %3 = bitcast i8* %0 to %List*
  ret %List* %3
}

; Function Attrs: nounwind
define noalias %List* @Nil() local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 8)
  %1 = bitcast i8* %0 to %List*
  ret %List* %1
}

; Function Attrs: nounwind
define noalias %Maybe* @Just(i8* %a) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 16)
  %.repack4 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack4 to i8**
  store i8* %a, i8** %1, align 8
  %2 = bitcast i8* %0 to %Maybe*
  ret %Maybe* %2
}

; Function Attrs: nounwind
define noalias %Maybe* @Nothing() local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 8)
  %1 = bitcast i8* %0 to %Maybe*
  ret %Maybe* %1
}

; Function Attrs: nounwind
define noalias %Ordering* @LE() local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 8)
  %1 = bitcast i8* %0 to %Ordering*
  ret %Ordering* %1
}

; Function Attrs: nounwind
define noalias %Ordering* @GE() local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 8)
  %1 = bitcast i8* %0 to %Ordering*
  ret %Ordering* %1
}

; Function Attrs: nounwind
define noalias %Ordering* @EQ() local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 8)
  %1 = bitcast i8* %0 to %Ordering*
  ret %Ordering* %1
}

; Function Attrs: nounwind
define noalias %Tuple* @T(i8* %a, i8* %a1) local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 24)
  %.repack6 = getelementptr inbounds i8, i8* %0, i64 8
  %1 = bitcast i8* %.repack6 to i8**
  store i8* %a, i8** %1, align 8
  %.repack8 = getelementptr inbounds i8, i8* %0, i64 16
  %2 = bitcast i8* %.repack8 to i8**
  store i8* %a1, i8** %2, align 8
  %3 = bitcast i8* %0 to %Tuple*
  ret %Tuple* %3
}

; Function Attrs: nounwind
define noalias %Unit* @MkUnit() local_unnamed_addr #0 {
entryC:
  %0 = tail call i8* @malloc(i64 8)
  %1 = bitcast i8* %0 to %Unit*
  ret %Unit* %1
}

; Function Attrs: nounwind
define noalias %Ord* @OrdInt() local_unnamed_addr #0 {
entry:
  %0 = tail call %Eq* @EqInt()
  %1 = tail call i8* @malloc(i64 24)
  %2 = bitcast i8* %1 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Ordering* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted0 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %2, align 8
  %3 = tail call i8* @malloc(i64 24)
  %4 = bitcast i8* %3 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Bool* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted1 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %4, align 8
  %5 = tail call i8* @malloc(i64 24)
  %6 = bitcast i8* %5 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Bool* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted2 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %6, align 8
  %7 = tail call i8* @malloc(i64 24)
  %8 = bitcast i8* %7 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Bool* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted3 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %8, align 8
  %9 = tail call i8* @malloc(i64 24)
  %10 = bitcast i8* %9 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Bool* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted4 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %10, align 8
  %11 = tail call i8* @malloc(i64 24)
  %12 = bitcast i8* %11 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Int* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted5 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %12, align 8
  %13 = tail call i8* @malloc(i64 24)
  %14 = bitcast i8* %13 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Int* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted6 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %14, align 8
  %15 = tail call %Ord* @MkOrd(%Eq* %0, { i8*, i8, [1 x i8*] }* %2, { i8*, i8, [1 x i8*] }* %4, { i8*, i8, [1 x i8*] }* %6, { i8*, i8, [1 x i8*] }* %8, { i8*, i8, [1 x i8*] }* %10, { i8*, i8, [1 x i8*] }* %12, { i8*, i8, [1 x i8*] }* %14)
  ret %Ord* %15
}

; Function Attrs: nounwind
define noalias %Eq* @EqBool() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Bool* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted7 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Eq* @MkEq({ i8*, i8, [1 x i8*] }* %1)
  ret %Eq* %2
}

; Function Attrs: nounwind
define noalias %Eq* @EqInt() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Bool* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted8 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Eq* @MkEq({ i8*, i8, [1 x i8*] }* %1)
  ret %Eq* %2
}

; Function Attrs: nounwind
define noalias %Foldable* @FoldableMaybe() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (i8* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted9 to i8*), i8 3, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call i8* @malloc(i64 24)
  %3 = bitcast i8* %2 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (i8* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted10 to i8*), i8 3, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %3, align 8
  %4 = tail call %Foldable* @MkFoldable({ i8*, i8, [1 x i8*] }* %1, { i8*, i8, [1 x i8*] }* %3)
  ret %Foldable* %4
}

; Function Attrs: nounwind
define noalias %Foldable* @FoldableList() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (i8* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted11 to i8*), i8 3, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call i8* @malloc(i64 24)
  %3 = bitcast i8* %2 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (i8* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted12 to i8*), i8 3, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %3, align 8
  %4 = tail call %Foldable* @MkFoldable({ i8*, i8, [1 x i8*] }* %1, { i8*, i8, [1 x i8*] }* %3)
  ret %Foldable* %4
}

; Function Attrs: nounwind
define noalias %Functor* @FunctorEither_a() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Either* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted13 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Functor* @MkFunctor({ i8*, i8, [1 x i8*] }* %1)
  ret %Functor* %2
}

; Function Attrs: nounwind
define noalias %Functor* @FunctorList() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%List* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted14 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Functor* @MkFunctor({ i8*, i8, [1 x i8*] }* %1)
  ret %Functor* %2
}

; Function Attrs: nounwind
define noalias %Group* @GroupInt() local_unnamed_addr #0 {
entry:
  %0 = tail call %Monoid* @MonoidInt()
  %1 = tail call i8* @malloc(i64 24)
  %2 = bitcast i8* %1 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Int* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted15 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %2, align 8
  %3 = tail call %Group* @MkGroup(%Monoid* %0, { i8*, i8, [1 x i8*] }* %2)
  ret %Group* %3
}

; Function Attrs: nounwind
define noalias %Monoid* @MonoidInt() local_unnamed_addr #0 {
entry:
  %0 = tail call %Semigroup* @SemigroupInt()
  %1 = tail call %Int* @mkInt(i64 0)
  %2 = bitcast %Int* %1 to i8*
  %3 = tail call %Monoid* @MkMonoid(%Semigroup* %0, i8* %2)
  ret %Monoid* %3
}

; Function Attrs: nounwind
define noalias %Semigroup* @SemigroupList_a() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%List* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted16 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Semigroup* @MkSemigroup({ i8*, i8, [1 x i8*] }* %1)
  ret %Semigroup* %2
}

; Function Attrs: nounwind
define noalias %Semigroup* @SemigroupMaybe_a() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Maybe* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted17 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Semigroup* @MkSemigroup({ i8*, i8, [1 x i8*] }* %1)
  ret %Semigroup* %2
}

; Function Attrs: nounwind
define noalias %Semigroup* @SemigroupString() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%String* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted18 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Semigroup* @MkSemigroup({ i8*, i8, [1 x i8*] }* %1)
  ret %Semigroup* %2
}

; Function Attrs: nounwind
define noalias %Semigroup* @SemigroupInt() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Int* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted19 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Semigroup* @MkSemigroup({ i8*, i8, [1 x i8*] }* %1)
  ret %Semigroup* %2
}

define noalias %Show* @callClosureShowMaybe_a({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Show**
  %2 = load %Show*, %Show** %1, align 8
  %3 = tail call %Show* @ShowMaybe_a(%Show* %2)
  ret %Show* %3
}

define noalias %Show* @ShowMaybe_a(%Show* %x1) local_unnamed_addr {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%String* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted21 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = bitcast %Show* %x1 to i8*
  %3 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %1, i8* %2)
  %4 = bitcast i8* %3 to { i8*, i8, [1 x i8*] }*
  %5 = tail call %Show* @MkShow({ i8*, i8, [1 x i8*] }* %4)
  ret %Show* %5
}

; Function Attrs: nounwind
define noalias %Show* @ShowBool() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%String* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted22 to i8*), i8 1, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Show* @MkShow({ i8*, i8, [1 x i8*] }* %1)
  ret %Show* %2
}

; Function Attrs: nounwind
define noalias %Show* @ShowInt() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%String* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted23 to i8*), i8 1, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = tail call %Show* @MkShow({ i8*, i8, [1 x i8*] }* %1)
  ret %Show* %2
}

define noalias %Show* @callClosureShowList_a({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Show**
  %2 = load %Show*, %Show** %1, align 8
  %3 = tail call %Show* @ShowList_a(%Show* %2)
  ret %Show* %3
}

define noalias %Show* @ShowList_a(%Show* %x1) local_unnamed_addr {
entry:
  %0 = tail call i8* @malloc(i64 24)
  %1 = bitcast i8* %0 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%String* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted25 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %1, align 8
  %2 = bitcast %Show* %x1 to i8*
  %3 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %1, i8* %2)
  %4 = bitcast i8* %3 to { i8*, i8, [1 x i8*] }*
  %5 = tail call %Show* @MkShow({ i8*, i8, [1 x i8*] }* %4)
  ret %Show* %5
}

define { i8*, i8, [1 x i8*] }* @callClosurenull({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Foldable**
  %2 = load %Foldable*, %Foldable** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @null(%Foldable* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

define { i8*, i8, [1 x i8*] }* @null(%Foldable* nocapture readonly %x1) local_unnamed_addr {
entry:
  %0 = tail call %Bool* @False()
  %1 = bitcast %Bool* %0 to i8*
  %2 = tail call { i8*, i8, [1 x i8*] }* @const(i8* %1)
  %3 = bitcast { i8*, i8, [1 x i8*] }* %2 to i8*
  %4 = tail call { i8*, i8, [1 x i8*] }* @const(i8* %3)
  %5 = tail call { i8*, i8, [1 x i8*] }* @foldr(%Foldable* %x1)
  %6 = tail call %Bool* @True()
  %7 = bitcast { i8*, i8, [1 x i8*] }* %4 to i8*
  %8 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %5, i8* %7)
  %9 = bitcast i8* %8 to { i8*, i8, [1 x i8*] }*
  %10 = bitcast %Bool* %6 to i8*
  %11 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %9, i8* %10)
  %12 = bitcast i8* %11 to { i8*, i8, [1 x i8*] }*
  ret { i8*, i8, [1 x i8*] }* %12
}

define { i8*, i8, [1 x i8*] }* @callClosureconst({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = load i8*, i8** %0, align 8
  %2 = tail call { i8*, i8, [1 x i8*] }* @const(i8* %1)
  ret { i8*, i8, [1 x i8*] }* %2
}

define { i8*, i8, [1 x i8*] }* @const(i8*) local_unnamed_addr {
entry:
  %1 = tail call i8* @malloc(i64 24)
  %2 = bitcast i8* %1 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (i8* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted27 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %2, align 8
  %3 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %2, i8* %0)
  %4 = bitcast i8* %3 to { i8*, i8, [1 x i8*] }*
  ret { i8*, i8, [1 x i8*] }* %4
}

; Function Attrs: nounwind
define noalias %Maybe* @callClosurehead({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = tail call %Maybe* @head(%List* %2)
  ret %Maybe* %3
}

; Function Attrs: nounwind
define noalias %Maybe* @head(%List* nocapture readonly %x1) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %List, %List* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %branchNil

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x1, i64 1
  %2 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %3 = tail call %Maybe* @Just(i8* %.unpack2)
  br label %switch_return

branchNil:                                        ; preds = %entry
  %4 = tail call %Maybe* @Nothing()
  br label %switch_return

switch_return:                                    ; preds = %branchNil, %branchCons
  %5 = phi %Maybe* [ %3, %branchCons ], [ %4, %branchNil ]
  ret %Maybe* %5
}

; Function Attrs: nounwind
define noalias %Maybe* @callClosureinit({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = tail call %Maybe* @init(%List* %2)
  ret %Maybe* %3
}

; Function Attrs: nounwind
define noalias %Maybe* @init(%List* nocapture readonly %x1) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %List, %List* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %branchNil1

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x1, i64 1
  %2 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %.elt3 = getelementptr inbounds %List, %List* %x1, i64 2
  %3 = bitcast %List* %.elt3 to %List**
  %.unpack4 = load %List*, %List** %3, align 8
  %4 = getelementptr %List, %List* %.unpack4, i64 0, i32 0
  %5 = load i64, i64* %4, align 8
  %cond = icmp eq i64 %5, 1
  br i1 %cond, label %branchNil, label %trivial_branch

branchNil:                                        ; preds = %branchCons
  %6 = tail call %List* @Nil()
  br label %switch_return1

trivial_branch:                                   ; preds = %branchCons
  %7 = tail call %Maybe* @init(%List* %.unpack4)
  %8 = getelementptr %Maybe, %Maybe* %7, i64 0, i32 0
  %9 = load i64, i64* %8, align 8
  %switch9 = icmp eq i64 %9, 1
  br i1 %switch9, label %branchJust, label %branchNothing

branchJust:                                       ; preds = %trivial_branch
  %.elt5 = getelementptr inbounds %Maybe, %Maybe* %7, i64 1
  %10 = bitcast %Maybe* %.elt5 to %List**
  %.unpack67 = load %List*, %List** %10, align 8
  br label %switch_return

branchNothing:                                    ; preds = %trivial_branch
  %11 = tail call %List* @Nil()
  br label %switch_return

switch_return:                                    ; preds = %branchNothing, %branchJust
  %.sink = phi %List* [ %11, %branchNothing ], [ %.unpack67, %branchJust ]
  %12 = tail call %List* @Cons(i8* %.unpack2, %List* %.sink)
  br label %switch_return1

switch_return1:                                   ; preds = %switch_return, %branchNil
  %.sink12 = phi %List* [ %12, %switch_return ], [ %6, %branchNil ]
  %13 = bitcast %List* %.sink12 to i8*
  %14 = tail call %Maybe* @Just(i8* %13)
  br label %switch_return2

branchNil1:                                       ; preds = %entry
  %15 = tail call %Maybe* @Nothing()
  br label %switch_return2

switch_return2:                                   ; preds = %branchNil1, %switch_return1
  %16 = phi %Maybe* [ %14, %switch_return1 ], [ %15, %branchNil1 ]
  ret %Maybe* %16
}

; Function Attrs: nounwind
define noalias %List* @callClosureintersperse({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = load i8*, i8** %3, align 8
  %5 = tail call %List* @intersperse(i8* %4, %List* %2)
  ret %List* %5
}

; Function Attrs: nounwind
define noalias %List* @intersperse(i8* %x1, %List* nocapture readonly %x2) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %List, %List* %x2, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %branchNil1

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x2, i64 1
  %2 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %.elt3 = getelementptr inbounds %List, %List* %x2, i64 2
  %3 = bitcast %List* %.elt3 to %List**
  %.unpack4 = load %List*, %List** %3, align 8
  %4 = getelementptr %List, %List* %.unpack4, i64 0, i32 0
  %5 = load i64, i64* %4, align 8
  %cond = icmp eq i64 %5, 1
  br i1 %cond, label %branchNil, label %trivial_branch

branchNil:                                        ; preds = %branchCons
  %6 = tail call %List* @Nil()
  br label %switch_return

trivial_branch:                                   ; preds = %branchCons
  %7 = tail call %List* @intersperse(i8* %x1, %List* %.unpack4)
  %8 = tail call %List* @Cons(i8* %x1, %List* %7)
  br label %switch_return

switch_return:                                    ; preds = %trivial_branch, %branchNil
  %.sink = phi %List* [ %8, %trivial_branch ], [ %6, %branchNil ]
  %9 = tail call %List* @Cons(i8* %.unpack2, %List* %.sink)
  br label %switch_return1

branchNil1:                                       ; preds = %entry
  %10 = tail call %List* @Nil()
  br label %switch_return1

switch_return1:                                   ; preds = %branchNil1, %switch_return
  %11 = phi %List* [ %9, %switch_return ], [ %10, %branchNil1 ]
  ret %List* %11
}

; Function Attrs: nounwind
define noalias %Maybe* @callClosurelast({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = tail call %Maybe* @last(%List* %2)
  ret %Maybe* %3
}

; Function Attrs: nounwind
define noalias %Maybe* @last(%List* nocapture readonly %x1) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %List, %List* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %branchNil

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x1, i64 1
  %2 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %3 = tail call %Maybe* @Just(i8* %.unpack2)
  br label %switch_return

branchNil:                                        ; preds = %entry
  %4 = tail call %Maybe* @Nothing()
  br label %switch_return

switch_return:                                    ; preds = %branchNil, %branchCons
  %5 = phi %Maybe* [ %3, %branchCons ], [ %4, %branchNil ]
  ret %Maybe* %5
}

define %Int* @callClosurelength({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = load i8*, i8** %0, align 8
  %2 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %3 = bitcast i8** %2 to %Foldable**
  %4 = load %Foldable*, %Foldable** %3, align 8
  %5 = tail call %Int* @length(%Foldable* %4, i8* %1)
  ret %Int* %5
}

define %Int* @length(%Foldable* nocapture readonly %x1, i8*) local_unnamed_addr {
entry:
  %1 = tail call { i8*, i8, [1 x i8*] }* @foldl(%Foldable* %x1)
  %2 = tail call i8* @malloc(i64 24)
  %3 = bitcast i8* %2 to { i8*, i8, [1 x i8*] }*
  store { i8*, i8, [1 x i8*] } { i8* bitcast (%Int* ({ i8*, i8, [1 x i8*] }*)* @callClosurelifted28 to i8*), i8 2, [1 x i8*] undef }, { i8*, i8, [1 x i8*] }* %3, align 8
  %4 = tail call %Int* @mkInt(i64 0)
  %5 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %1, i8* %2)
  %6 = bitcast i8* %5 to { i8*, i8, [1 x i8*] }*
  %7 = bitcast %Int* %4 to i8*
  %8 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %6, i8* %7)
  %9 = bitcast i8* %8 to { i8*, i8, [1 x i8*] }*
  %10 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %9, i8* %0)
  %11 = bitcast i8* %10 to %Int*
  ret %Int* %11
}

define %String* @module_main() {
entry:
  %0 = tail call %Int* @mkInt(i64 1)
  %1 = tail call %List* @Nil()
  %2 = bitcast %Int* %0 to i8*
  %3 = tail call %List* @Cons(i8* %2, %List* %1)
  %4 = tail call %Int* @sum(%List* %3)
  %5 = tail call %Show* @ShowInt()
  %6 = tail call { i8*, i8, [1 x i8*] }* @show(%Show* %5)
  %7 = bitcast %Int* %4 to i8*
  %8 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %6, i8* %7)
  %9 = bitcast i8* %8 to %String*
  ret %String* %9
}

define %Int* @callClosuresum({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = tail call %Int* @sum(%List* %2)
  ret %Int* %3
}

define %Int* @sum(%List* nocapture readonly %x1) local_unnamed_addr {
entry:
  %0 = getelementptr %List, %List* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %branchNil

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x1, i64 1
  %2 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %.elt3 = getelementptr inbounds %List, %List* %x1, i64 2
  %3 = bitcast %List* %.elt3 to %List**
  %.unpack4 = load %List*, %List** %3, align 8
  %4 = tail call %Int* @sum(%List* %.unpack4)
  %5 = tail call %Semigroup* @SemigroupInt()
  %6 = tail call { i8*, i8, [1 x i8*] }* @plus(%Semigroup* %5)
  %7 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %6, i8* %.unpack2)
  %8 = bitcast i8* %7 to { i8*, i8, [1 x i8*] }*
  %9 = bitcast %Int* %4 to i8*
  %10 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %8, i8* %9)
  %11 = bitcast i8* %10 to %Int*
  ret %Int* %11

branchNil:                                        ; preds = %entry
  %12 = tail call %Int* @mkInt(i64 0)
  ret %Int* %12
}

; Function Attrs: nounwind
define noalias %Maybe* @callClosuretail({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = tail call %Maybe* @tail(%List* %2)
  ret %Maybe* %3
}

; Function Attrs: nounwind
define noalias %Maybe* @tail(%List* nocapture readonly %x1) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %List, %List* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %branchNil

branchCons:                                       ; preds = %entry
  %.elt3 = getelementptr inbounds %List, %List* %x1, i64 2
  %2 = bitcast %List* %.elt3 to i8**
  %.unpack45 = load i8*, i8** %2, align 8
  %3 = tail call %Maybe* @Just(i8* %.unpack45)
  br label %switch_return

branchNil:                                        ; preds = %entry
  %4 = tail call %Maybe* @Nothing()
  br label %switch_return

switch_return:                                    ; preds = %branchNil, %branchCons
  %5 = phi %Maybe* [ %3, %branchCons ], [ %4, %branchNil ]
  ret %Maybe* %5
}

; Function Attrs: nounwind
define noalias %Maybe* @callClosureuncons({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = tail call %Maybe* @uncons(%List* %2)
  ret %Maybe* %3
}

; Function Attrs: nounwind
define noalias %Maybe* @uncons(%List* nocapture readonly %x1) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %List, %List* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %branchNil

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x1, i64 1
  %2 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %.elt3 = getelementptr inbounds %List, %List* %x1, i64 2
  %3 = bitcast %List* %.elt3 to i8**
  %.unpack45 = load i8*, i8** %3, align 8
  %4 = tail call %Tuple* @T(i8* %.unpack2, i8* %.unpack45)
  %5 = bitcast %Tuple* %4 to i8*
  %6 = tail call %Maybe* @Just(i8* %5)
  br label %switch_return

branchNil:                                        ; preds = %entry
  %7 = tail call %Maybe* @Nothing()
  br label %switch_return

switch_return:                                    ; preds = %branchNil, %branchCons
  %8 = phi %Maybe* [ %6, %branchCons ], [ %7, %branchNil ]
  ret %Maybe* %8
}

define noalias %List* @callClosurezipWith({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %List**
  %5 = load %List*, %List** %4, align 8
  %6 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 2
  %7 = bitcast i8** %6 to { i8*, i8, [1 x i8*] }**
  %8 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %7, align 8
  %9 = tail call %List* @zipWith({ i8*, i8, [1 x i8*] }* %8, %List* %5, %List* %2)
  ret %List* %9
}

define noalias %List* @zipWith({ i8*, i8, [1 x i8*] }* %x1, %List* nocapture readonly %x2, %List* nocapture readonly %x3) local_unnamed_addr {
entry:
  %0 = getelementptr %List, %List* %x2, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %cond = icmp eq i64 %1, 1
  br i1 %cond, label %branchCons, label %switch_return1

branchCons:                                       ; preds = %entry
  %2 = getelementptr %List, %List* %x3, i64 0, i32 0
  %3 = load i64, i64* %2, align 8
  %cond1 = icmp eq i64 %3, 1
  br i1 %cond1, label %branchCons1, label %switch_return1

branchCons1:                                      ; preds = %branchCons
  %.elt2 = getelementptr inbounds %List, %List* %x2, i64 1
  %4 = bitcast %List* %.elt2 to i8**
  %.unpack3 = load i8*, i8** %4, align 8
  %.elt4 = getelementptr inbounds %List, %List* %x2, i64 2
  %5 = bitcast %List* %.elt4 to %List**
  %.unpack5 = load %List*, %List** %5, align 8
  %.elt6 = getelementptr inbounds %List, %List* %x3, i64 1
  %6 = bitcast %List* %.elt6 to i8**
  %.unpack7 = load i8*, i8** %6, align 8
  %.elt8 = getelementptr inbounds %List, %List* %x3, i64 2
  %7 = bitcast %List* %.elt8 to %List**
  %.unpack9 = load %List*, %List** %7, align 8
  %8 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %x1, i8* %.unpack3)
  %9 = bitcast i8* %8 to { i8*, i8, [1 x i8*] }*
  %10 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %9, i8* %.unpack7)
  %11 = tail call %List* @zipWith({ i8*, i8, [1 x i8*] }* %x1, %List* %.unpack5, %List* %.unpack9)
  %12 = tail call %List* @Cons(i8* %10, %List* %11)
  ret %List* %12

switch_return1:                                   ; preds = %branchCons, %entry
  %13 = tail call %List* @Nil()
  ret %List* %13
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurebind({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Monad**
  %2 = load %Monad*, %Monad** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @bind(%Monad* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @bind(%Monad* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt3 = getelementptr inbounds %Monad, %Monad* %x1, i64 2
  %0 = bitcast %Monad* %.elt3 to { i8*, i8, [1 x i8*] }**
  %.unpack4 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack4
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurepure({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Applicative**
  %2 = load %Applicative*, %Applicative** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @pure(%Applicative* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @pure(%Applicative* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt5 = getelementptr inbounds %Applicative, %Applicative* %x1, i64 3
  %0 = bitcast %Applicative* %.elt5 to { i8*, i8, [1 x i8*] }**
  %.unpack6 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack6
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosureap({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Applicative**
  %2 = load %Applicative*, %Applicative** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @ap(%Applicative* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @ap(%Applicative* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt3 = getelementptr inbounds %Applicative, %Applicative* %x1, i64 2
  %0 = bitcast %Applicative* %.elt3 to { i8*, i8, [1 x i8*] }**
  %.unpack4 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack4
}

; Function Attrs: norecurse nounwind readonly
define i8* @callClosureminBound({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Bounded**
  %2 = load %Bounded*, %Bounded** %1, align 8
  %3 = tail call i8* @minBound(%Bounded* %2)
  ret i8* %3
}

; Function Attrs: norecurse nounwind readonly
define i8* @minBound(%Bounded* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt3 = getelementptr inbounds %Bounded, %Bounded* %x1, i64 2
  %0 = bitcast %Bounded* %.elt3 to i8**
  %.unpack4 = load i8*, i8** %0, align 8
  ret i8* %.unpack4
}

; Function Attrs: norecurse nounwind readonly
define i8* @callClosuremaxBound({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Bounded**
  %2 = load %Bounded*, %Bounded** %1, align 8
  %3 = tail call i8* @maxBound(%Bounded* %2)
  ret i8* %3
}

; Function Attrs: norecurse nounwind readonly
define i8* @maxBound(%Bounded* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt1 = getelementptr inbounds %Bounded, %Bounded* %x1, i64 1
  %0 = bitcast %Bounded* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %0, align 8
  ret i8* %.unpack2
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosuretoEnum({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Enum**
  %2 = load %Enum*, %Enum** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @toEnum(%Enum* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @toEnum(%Enum* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt7 = getelementptr inbounds %Enum, %Enum* %x1, i64 4
  %0 = bitcast %Enum* %.elt7 to { i8*, i8, [1 x i8*] }**
  %.unpack8 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack8
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosuresucc({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Enum**
  %2 = load %Enum*, %Enum** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @succ(%Enum* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @succ(%Enum* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt5 = getelementptr inbounds %Enum, %Enum* %x1, i64 3
  %0 = bitcast %Enum* %.elt5 to { i8*, i8, [1 x i8*] }**
  %.unpack6 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack6
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurepred({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Enum**
  %2 = load %Enum*, %Enum** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @pred(%Enum* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @pred(%Enum* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt3 = getelementptr inbounds %Enum, %Enum* %x1, i64 2
  %0 = bitcast %Enum* %.elt3 to { i8*, i8, [1 x i8*] }**
  %.unpack4 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack4
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurefromEnum({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Enum**
  %2 = load %Enum*, %Enum** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @fromEnum(%Enum* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @fromEnum(%Enum* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt1 = getelementptr inbounds %Enum, %Enum* %x1, i64 1
  %0 = bitcast %Enum* %.elt1 to { i8*, i8, [1 x i8*] }**
  %.unpack2 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack2
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurefoldr({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Foldable**
  %2 = load %Foldable*, %Foldable** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @foldr(%Foldable* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @foldr(%Foldable* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt3 = getelementptr inbounds %Foldable, %Foldable* %x1, i64 2
  %0 = bitcast %Foldable* %.elt3 to { i8*, i8, [1 x i8*] }**
  %.unpack4 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack4
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurefoldl({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Foldable**
  %2 = load %Foldable*, %Foldable** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @foldl(%Foldable* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @foldl(%Foldable* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt1 = getelementptr inbounds %Foldable, %Foldable* %x1, i64 1
  %0 = bitcast %Foldable* %.elt1 to { i8*, i8, [1 x i8*] }**
  %.unpack2 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack2
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosuremin({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Ord**
  %2 = load %Ord*, %Ord** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @min(%Ord* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @min(%Ord* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt15 = getelementptr inbounds %Ord, %Ord* %x1, i64 8
  %0 = bitcast %Ord* %.elt15 to { i8*, i8, [1 x i8*] }**
  %.unpack16 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack16
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosuremax({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Ord**
  %2 = load %Ord*, %Ord** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @max(%Ord* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @max(%Ord* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt13 = getelementptr inbounds %Ord, %Ord* %x1, i64 7
  %0 = bitcast %Ord* %.elt13 to { i8*, i8, [1 x i8*] }**
  %.unpack14 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack14
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurelt({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Ord**
  %2 = load %Ord*, %Ord** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @lt(%Ord* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @lt(%Ord* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt11 = getelementptr inbounds %Ord, %Ord* %x1, i64 6
  %0 = bitcast %Ord* %.elt11 to { i8*, i8, [1 x i8*] }**
  %.unpack12 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack12
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosureleq({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Ord**
  %2 = load %Ord*, %Ord** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @leq(%Ord* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @leq(%Ord* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt9 = getelementptr inbounds %Ord, %Ord* %x1, i64 5
  %0 = bitcast %Ord* %.elt9 to { i8*, i8, [1 x i8*] }**
  %.unpack10 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack10
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosuregt({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Ord**
  %2 = load %Ord*, %Ord** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @gt(%Ord* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @gt(%Ord* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt7 = getelementptr inbounds %Ord, %Ord* %x1, i64 4
  %0 = bitcast %Ord* %.elt7 to { i8*, i8, [1 x i8*] }**
  %.unpack8 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack8
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosuregeq({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Ord**
  %2 = load %Ord*, %Ord** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @geq(%Ord* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @geq(%Ord* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt5 = getelementptr inbounds %Ord, %Ord* %x1, i64 3
  %0 = bitcast %Ord* %.elt5 to { i8*, i8, [1 x i8*] }**
  %.unpack6 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack6
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurecompare({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Ord**
  %2 = load %Ord*, %Ord** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @compare(%Ord* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @compare(%Ord* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt3 = getelementptr inbounds %Ord, %Ord* %x1, i64 2
  %0 = bitcast %Ord* %.elt3 to { i8*, i8, [1 x i8*] }**
  %.unpack4 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack4
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosureminus({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Group**
  %2 = load %Group*, %Group** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @minus(%Group* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @minus(%Group* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt3 = getelementptr inbounds %Group, %Group* %x1, i64 2
  %0 = bitcast %Group* %.elt3 to { i8*, i8, [1 x i8*] }**
  %.unpack4 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack4
}

; Function Attrs: norecurse nounwind readonly
define i8* @callClosurezero({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Monoid**
  %2 = load %Monoid*, %Monoid** %1, align 8
  %3 = tail call i8* @zero(%Monoid* %2)
  ret i8* %3
}

; Function Attrs: norecurse nounwind readonly
define i8* @zero(%Monoid* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt3 = getelementptr inbounds %Monoid, %Monoid* %x1, i64 2
  %0 = bitcast %Monoid* %.elt3 to i8**
  %.unpack4 = load i8*, i8** %0, align 8
  ret i8* %.unpack4
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosureplus({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Semigroup**
  %2 = load %Semigroup*, %Semigroup** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @plus(%Semigroup* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @plus(%Semigroup* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt1 = getelementptr inbounds %Semigroup, %Semigroup* %x1, i64 1
  %0 = bitcast %Semigroup* %.elt1 to { i8*, i8, [1 x i8*] }**
  %.unpack2 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack2
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosurefmap({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Functor**
  %2 = load %Functor*, %Functor** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @fmap(%Functor* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @fmap(%Functor* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt1 = getelementptr inbounds %Functor, %Functor* %x1, i64 1
  %0 = bitcast %Functor* %.elt1 to { i8*, i8, [1 x i8*] }**
  %.unpack2 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack2
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosureshow({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Show**
  %2 = load %Show*, %Show** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @show(%Show* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @show(%Show* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt1 = getelementptr inbounds %Show, %Show* %x1, i64 1
  %0 = bitcast %Show* %.elt1 to { i8*, i8, [1 x i8*] }**
  %.unpack2 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack2
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @callClosureeq({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) local_unnamed_addr #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Eq**
  %2 = load %Eq*, %Eq** %1, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @eq(%Eq* %2)
  ret { i8*, i8, [1 x i8*] }* %3
}

; Function Attrs: norecurse nounwind readonly
define { i8*, i8, [1 x i8*] }* @eq(%Eq* nocapture readonly %x1) local_unnamed_addr #1 {
entry:
  %.elt1 = getelementptr inbounds %Eq, %Eq* %x1, i64 1
  %0 = bitcast %Eq* %.elt1 to { i8*, i8, [1 x i8*] }**
  %.unpack2 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %0, align 8
  ret { i8*, i8, [1 x i8*] }* %.unpack2
}

define noalias %Ordering* @callClosurelifted0({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Ordering* @lifted0(%Int* %5, %Int* %2)
  ret %Ordering* %6
}

define noalias %Ordering* @lifted0(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Ord* @OrdInt()
  %1 = tail call { i8*, i8, [1 x i8*] }* @lt(%Ord* %0)
  %2 = bitcast %Int* %x1 to i8*
  %3 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %1, i8* %2)
  %4 = bitcast i8* %3 to { i8*, i8, [1 x i8*] }*
  %5 = bitcast %Int* %x2 to i8*
  %6 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %4, i8* %5)
  %7 = bitcast i8* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %switch = icmp eq i64 %8, 1
  br i1 %switch, label %branchTrue, label %branchFalse

branchTrue:                                       ; preds = %entry
  %9 = tail call %Ordering* @LE()
  br label %switch_return1

branchFalse:                                      ; preds = %entry
  %10 = tail call %Ord* @OrdInt()
  %11 = tail call { i8*, i8, [1 x i8*] }* @gt(%Ord* %10)
  %12 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %11, i8* %2)
  %13 = bitcast i8* %12 to { i8*, i8, [1 x i8*] }*
  %14 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %13, i8* %5)
  %15 = bitcast i8* %14 to i64*
  %16 = load i64, i64* %15, align 8
  %switch2 = icmp eq i64 %16, 1
  br i1 %switch2, label %branchTrue1, label %branchFalse1

branchTrue1:                                      ; preds = %branchFalse
  %17 = tail call %Ordering* @GE()
  br label %switch_return1

branchFalse1:                                     ; preds = %branchFalse
  %18 = tail call %Ordering* @EQ()
  br label %switch_return1

switch_return1:                                   ; preds = %branchTrue1, %branchFalse1, %branchTrue
  %19 = phi %Ordering* [ %9, %branchTrue ], [ %17, %branchTrue1 ], [ %18, %branchFalse1 ]
  ret %Ordering* %19
}

define %Bool* @callClosurelifted1({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Bool* @lifted1(%Int* %5, %Int* %2)
  ret %Bool* %6
}

define %Bool* @lifted1(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Bool* @geqInt(%Int* %x1, %Int* %x2)
  ret %Bool* %0
}

define %Bool* @callClosurelifted2({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Bool* @lifted2(%Int* %5, %Int* %2)
  ret %Bool* %6
}

define %Bool* @lifted2(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Bool* @gtInt(%Int* %x1, %Int* %x2)
  ret %Bool* %0
}

define %Bool* @callClosurelifted3({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Bool* @lifted3(%Int* %5, %Int* %2)
  ret %Bool* %6
}

define %Bool* @lifted3(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Bool* @leqInt(%Int* %x1, %Int* %x2)
  ret %Bool* %0
}

define %Bool* @callClosurelifted4({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Bool* @lifted4(%Int* %5, %Int* %2)
  ret %Bool* %6
}

define %Bool* @lifted4(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Bool* @ltInt(%Int* %x1, %Int* %x2)
  ret %Bool* %0
}

define %Int* @callClosurelifted5({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Int* @lifted5(%Int* %5, %Int* %2)
  ret %Int* %6
}

define %Int* @lifted5(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Bool* @geqInt(%Int* %x1, %Int* %x2)
  %1 = getelementptr %Bool, %Bool* %0, i64 0, i32 0
  %2 = load i64, i64* %1, align 8
  %switch = icmp eq i64 %2, 1
  %x1.x2 = select i1 %switch, %Int* %x1, %Int* %x2
  ret %Int* %x1.x2
}

define %Int* @callClosurelifted6({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Int* @lifted6(%Int* %5, %Int* %2)
  ret %Int* %6
}

define %Int* @lifted6(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Bool* @leqInt(%Int* %x1, %Int* %x2)
  %1 = getelementptr %Bool, %Bool* %0, i64 0, i32 0
  %2 = load i64, i64* %1, align 8
  %switch = icmp eq i64 %2, 1
  %x1.x2 = select i1 %switch, %Int* %x1, %Int* %x2
  ret %Int* %x1.x2
}

; Function Attrs: nounwind
define noalias %Bool* @callClosurelifted7({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Bool**
  %2 = load %Bool*, %Bool** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Bool**
  %5 = load %Bool*, %Bool** %4, align 8
  %6 = tail call %Bool* @lifted7(%Bool* %5, %Bool* %2)
  ret %Bool* %6
}

; Function Attrs: nounwind
define noalias %Bool* @lifted7(%Bool* nocapture readonly %x1, %Bool* nocapture readonly %x2) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %Bool, %Bool* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch2 [
    i64 1, label %branchFalse
    i64 2, label %branchTrue
  ]

branchFalse:                                      ; preds = %entry
  %2 = getelementptr %Bool, %Bool* %x2, i64 0, i32 0
  %3 = load i64, i64* %2, align 8
  %cond1 = icmp eq i64 %3, 1
  br i1 %cond1, label %branchFalse1, label %trivial_branch

branchFalse1:                                     ; preds = %branchFalse
  %4 = tail call %Bool* @True()
  br label %switch_return2

trivial_branch:                                   ; preds = %branchFalse
  %5 = tail call %Bool* @False()
  br label %switch_return2

branchTrue:                                       ; preds = %entry
  %6 = getelementptr %Bool, %Bool* %x2, i64 0, i32 0
  %7 = load i64, i64* %6, align 8
  %cond = icmp eq i64 %7, 1
  br i1 %cond, label %branchTrue1, label %trivial_branch1

branchTrue1:                                      ; preds = %branchTrue
  %8 = tail call %Bool* @True()
  br label %switch_return2

trivial_branch1:                                  ; preds = %branchTrue
  %9 = tail call %Bool* @False()
  br label %switch_return2

trivial_branch2:                                  ; preds = %entry
  %10 = tail call %Bool* @False()
  br label %switch_return2

switch_return2:                                   ; preds = %branchTrue1, %trivial_branch1, %branchFalse1, %trivial_branch, %trivial_branch2
  %11 = phi %Bool* [ %10, %trivial_branch2 ], [ %5, %trivial_branch ], [ %4, %branchFalse1 ], [ %9, %trivial_branch1 ], [ %8, %branchTrue1 ]
  ret %Bool* %11
}

define %Bool* @callClosurelifted8({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Bool* @lifted8(%Int* %5, %Int* %2)
  ret %Bool* %6
}

define %Bool* @lifted8(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Bool* @eqInt(%Int* %x1, %Int* %x2)
  ret %Bool* %0
}

define i8* @callClosurelifted9({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Maybe**
  %2 = load %Maybe*, %Maybe** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = load i8*, i8** %3, align 8
  %5 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 2
  %6 = bitcast i8** %5 to { i8*, i8, [1 x i8*] }**
  %7 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %6, align 8
  %8 = tail call i8* @lifted9({ i8*, i8, [1 x i8*] }* %7, i8* %4, %Maybe* %2)
  ret i8* %8
}

define i8* @lifted9({ i8*, i8, [1 x i8*] }* %x1, i8*, %Maybe* nocapture readonly %x3) local_unnamed_addr {
entry:
  %1 = getelementptr %Maybe, %Maybe* %x3, i64 0, i32 0
  %2 = load i64, i64* %1, align 8
  %switch = icmp eq i64 %2, 1
  br i1 %switch, label %branchJust, label %switch_return

branchJust:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %Maybe, %Maybe* %x3, i64 1
  %3 = bitcast %Maybe* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %3, align 8
  %4 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %x1, i8* %0)
  %5 = bitcast i8* %4 to { i8*, i8, [1 x i8*] }*
  %6 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %5, i8* %.unpack2)
  br label %switch_return

switch_return:                                    ; preds = %entry, %branchJust
  %7 = phi i8* [ %6, %branchJust ], [ %0, %entry ]
  ret i8* %7
}

define i8* @callClosurelifted10({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Maybe**
  %2 = load %Maybe*, %Maybe** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = load i8*, i8** %3, align 8
  %5 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 2
  %6 = bitcast i8** %5 to { i8*, i8, [1 x i8*] }**
  %7 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %6, align 8
  %8 = tail call i8* @lifted10({ i8*, i8, [1 x i8*] }* %7, i8* %4, %Maybe* %2)
  ret i8* %8
}

define i8* @lifted10({ i8*, i8, [1 x i8*] }* %x1, i8*, %Maybe* nocapture readonly %x3) local_unnamed_addr {
entry:
  %1 = getelementptr %Maybe, %Maybe* %x3, i64 0, i32 0
  %2 = load i64, i64* %1, align 8
  %switch = icmp eq i64 %2, 1
  br i1 %switch, label %branchJust, label %switch_return

branchJust:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %Maybe, %Maybe* %x3, i64 1
  %3 = bitcast %Maybe* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %3, align 8
  %4 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %x1, i8* %.unpack2)
  %5 = bitcast i8* %4 to { i8*, i8, [1 x i8*] }*
  %6 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %5, i8* %0)
  br label %switch_return

switch_return:                                    ; preds = %entry, %branchJust
  %7 = phi i8* [ %6, %branchJust ], [ %0, %entry ]
  ret i8* %7
}

define i8* @callClosurelifted11({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = load i8*, i8** %3, align 8
  %5 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 2
  %6 = bitcast i8** %5 to { i8*, i8, [1 x i8*] }**
  %7 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %6, align 8
  %8 = tail call i8* @lifted11({ i8*, i8, [1 x i8*] }* %7, i8* %4, %List* %2)
  ret i8* %8
}

define i8* @lifted11({ i8*, i8, [1 x i8*] }* %x1, i8*, %List* nocapture readonly %x3) local_unnamed_addr {
entry:
  %1 = getelementptr %List, %List* %x3, i64 0, i32 0
  %2 = load i64, i64* %1, align 8
  %switch = icmp eq i64 %2, 1
  br i1 %switch, label %branchCons, label %switch_return

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x3, i64 1
  %3 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %3, align 8
  %.elt3 = getelementptr inbounds %List, %List* %x3, i64 2
  %4 = bitcast %List* %.elt3 to i8**
  %.unpack45 = load i8*, i8** %4, align 8
  %5 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %x1, i8* %0)
  %6 = bitcast i8* %5 to { i8*, i8, [1 x i8*] }*
  %7 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %6, i8* %.unpack2)
  %8 = tail call %Foldable* @FoldableList()
  %9 = tail call { i8*, i8, [1 x i8*] }* @foldl(%Foldable* %8)
  %10 = bitcast { i8*, i8, [1 x i8*] }* %x1 to i8*
  %11 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %9, i8* %10)
  %12 = bitcast i8* %11 to { i8*, i8, [1 x i8*] }*
  %13 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %12, i8* %7)
  %14 = bitcast i8* %13 to { i8*, i8, [1 x i8*] }*
  %15 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %14, i8* %.unpack45)
  br label %switch_return

switch_return:                                    ; preds = %entry, %branchCons
  %16 = phi i8* [ %15, %branchCons ], [ %0, %entry ]
  ret i8* %16
}

define i8* @callClosurelifted12({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = load i8*, i8** %3, align 8
  %5 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 2
  %6 = bitcast i8** %5 to { i8*, i8, [1 x i8*] }**
  %7 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %6, align 8
  %8 = tail call i8* @lifted12({ i8*, i8, [1 x i8*] }* %7, i8* %4, %List* %2)
  ret i8* %8
}

define i8* @lifted12({ i8*, i8, [1 x i8*] }* %x1, i8*, %List* nocapture readonly %x3) local_unnamed_addr {
entry:
  %1 = getelementptr %List, %List* %x3, i64 0, i32 0
  %2 = load i64, i64* %1, align 8
  %switch = icmp eq i64 %2, 1
  br i1 %switch, label %branchCons, label %switch_return

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x3, i64 1
  %3 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %3, align 8
  %.elt3 = getelementptr inbounds %List, %List* %x3, i64 2
  %4 = bitcast %List* %.elt3 to i8**
  %.unpack45 = load i8*, i8** %4, align 8
  %5 = tail call %Foldable* @FoldableList()
  %6 = tail call { i8*, i8, [1 x i8*] }* @foldr(%Foldable* %5)
  %7 = bitcast { i8*, i8, [1 x i8*] }* %x1 to i8*
  %8 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %6, i8* %7)
  %9 = bitcast i8* %8 to { i8*, i8, [1 x i8*] }*
  %10 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %9, i8* %0)
  %11 = bitcast i8* %10 to { i8*, i8, [1 x i8*] }*
  %12 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %11, i8* %.unpack45)
  %13 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %x1, i8* %.unpack2)
  %14 = bitcast i8* %13 to { i8*, i8, [1 x i8*] }*
  %15 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %14, i8* %12)
  br label %switch_return

switch_return:                                    ; preds = %entry, %branchCons
  %16 = phi i8* [ %15, %branchCons ], [ %0, %entry ]
  ret i8* %16
}

define noalias %Either* @callClosurelifted13({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Either**
  %2 = load %Either*, %Either** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to { i8*, i8, [1 x i8*] }**
  %5 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %4, align 8
  %6 = tail call %Either* @lifted13({ i8*, i8, [1 x i8*] }* %5, %Either* %2)
  ret %Either* %6
}

define noalias %Either* @lifted13({ i8*, i8, [1 x i8*] }* %x1, %Either* nocapture readonly %x2) local_unnamed_addr {
entry:
  %0 = getelementptr %Either, %Either* %x2, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %cond = icmp eq i64 %1, 1
  %.elt3 = getelementptr inbounds %Either, %Either* %x2, i64 1
  %2 = bitcast %Either* %.elt3 to i8**
  %.unpack4 = load i8*, i8** %2, align 8
  br i1 %cond, label %branchRight, label %branchLeft

branchRight:                                      ; preds = %entry
  %3 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %x1, i8* %.unpack4)
  %4 = tail call %Either* @Right(i8* %3)
  br label %switch_return1

branchLeft:                                       ; preds = %entry
  %5 = tail call %Either* @Left(i8* %.unpack4)
  br label %switch_return1

switch_return1:                                   ; preds = %branchLeft, %branchRight
  %6 = phi %Either* [ %5, %branchLeft ], [ %4, %branchRight ]
  ret %Either* %6
}

define noalias %List* @callClosurelifted14({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to { i8*, i8, [1 x i8*] }**
  %5 = load { i8*, i8, [1 x i8*] }*, { i8*, i8, [1 x i8*] }** %4, align 8
  %6 = tail call %List* @lifted14({ i8*, i8, [1 x i8*] }* %5, %List* %2)
  ret %List* %6
}

define noalias %List* @lifted14({ i8*, i8, [1 x i8*] }* %x1, %List* nocapture readonly %x2) local_unnamed_addr {
entry:
  %0 = getelementptr %List, %List* %x2, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %branchNil

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x2, i64 1
  %2 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %.elt3 = getelementptr inbounds %List, %List* %x2, i64 2
  %3 = bitcast %List* %.elt3 to i8**
  %.unpack45 = load i8*, i8** %3, align 8
  %4 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %x1, i8* %.unpack2)
  %5 = tail call %Functor* @FunctorList()
  %6 = tail call { i8*, i8, [1 x i8*] }* @fmap(%Functor* %5)
  %7 = bitcast { i8*, i8, [1 x i8*] }* %x1 to i8*
  %8 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %6, i8* %7)
  %9 = bitcast i8* %8 to { i8*, i8, [1 x i8*] }*
  %10 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %9, i8* %.unpack45)
  %11 = bitcast i8* %10 to %List*
  %12 = tail call %List* @Cons(i8* %4, %List* %11)
  br label %switch_return

branchNil:                                        ; preds = %entry
  %13 = tail call %List* @Nil()
  br label %switch_return

switch_return:                                    ; preds = %branchNil, %branchCons
  %14 = phi %List* [ %12, %branchCons ], [ %13, %branchNil ]
  ret %List* %14
}

define %Int* @callClosurelifted15({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Int* @lifted15(%Int* %5, %Int* %2)
  ret %Int* %6
}

define %Int* @lifted15(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Int* @minusInt(%Int* %x1, %Int* %x2)
  ret %Int* %0
}

define %List* @callClosurelifted16({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %List**
  %5 = load %List*, %List** %4, align 8
  %6 = tail call %List* @lifted16(%List* %5, %List* %2)
  ret %List* %6
}

define %List* @lifted16(%List* nocapture readonly %x1, %List* %x2) local_unnamed_addr {
entry:
  %0 = getelementptr %List, %List* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchCons, label %switch_return1

branchCons:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %List, %List* %x1, i64 1
  %2 = bitcast %List* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %.elt3 = getelementptr inbounds %List, %List* %x1, i64 2
  %3 = bitcast %List* %.elt3 to %List**
  %.unpack4 = load %List*, %List** %3, align 8
  %4 = getelementptr %List, %List* %x2, i64 0, i32 0
  %5 = load i64, i64* %4, align 8
  %cond = icmp eq i64 %5, 1
  br i1 %cond, label %switch_return, label %trivial_branch

trivial_branch:                                   ; preds = %branchCons
  %6 = tail call %Semigroup* @SemigroupList_a()
  %7 = tail call { i8*, i8, [1 x i8*] }* @plus(%Semigroup* %6)
  %8 = bitcast %List* %.unpack4 to i8*
  %9 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %7, i8* %8)
  %10 = bitcast i8* %9 to { i8*, i8, [1 x i8*] }*
  %11 = bitcast %List* %x2 to i8*
  %12 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %10, i8* %11)
  %13 = bitcast i8* %12 to %List*
  br label %switch_return

switch_return:                                    ; preds = %branchCons, %trivial_branch
  %.sink = phi %List* [ %13, %trivial_branch ], [ %.unpack4, %branchCons ]
  %14 = tail call %List* @Cons(i8* %.unpack2, %List* %.sink)
  br label %switch_return1

switch_return1:                                   ; preds = %entry, %switch_return
  %15 = phi %List* [ %14, %switch_return ], [ %x2, %entry ]
  ret %List* %15
}

; Function Attrs: nounwind
define %Maybe* @callClosurelifted17({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Maybe**
  %2 = load %Maybe*, %Maybe** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Maybe**
  %5 = load %Maybe*, %Maybe** %4, align 8
  %6 = tail call %Maybe* @lifted17(%Maybe* %5, %Maybe* %2)
  ret %Maybe* %6
}

; Function Attrs: nounwind
define %Maybe* @lifted17(%Maybe* nocapture readonly %x1, %Maybe* readnone %x2) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %Maybe, %Maybe* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchJust, label %switch_return

branchJust:                                       ; preds = %entry
  %.elt1 = getelementptr inbounds %Maybe, %Maybe* %x1, i64 1
  %2 = bitcast %Maybe* %.elt1 to i8**
  %.unpack2 = load i8*, i8** %2, align 8
  %3 = tail call %Maybe* @Just(i8* %.unpack2)
  br label %switch_return

switch_return:                                    ; preds = %entry, %branchJust
  %4 = phi %Maybe* [ %3, %branchJust ], [ %x2, %entry ]
  ret %Maybe* %4
}

define %String* @callClosurelifted18({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %String**
  %2 = load %String*, %String** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %String**
  %5 = load %String*, %String** %4, align 8
  %6 = tail call %String* @lifted18(%String* %5, %String* %2)
  ret %String* %6
}

define %String* @lifted18(%String* %x1, %String* %x2) local_unnamed_addr {
entry:
  %0 = tail call %String* @plusStr(%String* %x1, %String* %x2)
  ret %String* %0
}

define %Int* @callClosurelifted19({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = tail call %Int* @lifted19(%Int* %5, %Int* %2)
  ret %Int* %6
}

define %Int* @lifted19(%Int* %x1, %Int* %x2) local_unnamed_addr {
entry:
  %0 = tail call %Int* @plusInt(%Int* %x1, %Int* %x2)
  ret %Int* %0
}

define %String* @callClosurelifted21({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Maybe**
  %2 = load %Maybe*, %Maybe** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Show**
  %5 = load %Show*, %Show** %4, align 8
  %6 = tail call %String* @lifted21(%Show* %5, %Maybe* %2)
  ret %String* %6
}

define %String* @lifted21(%Show* nocapture readonly %cvar20, %Maybe* nocapture readonly %x1) local_unnamed_addr {
entry:
  %0 = getelementptr %Maybe, %Maybe* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchJust, label %branchNothing

branchJust:                                       ; preds = %entry
  %.elt10 = getelementptr inbounds %Maybe, %Maybe* %x1, i64 1
  %2 = bitcast %Maybe* %.elt10 to i8**
  %.unpack11 = load i8*, i8** %2, align 8
  %3 = tail call { i8*, i8, [1 x i8*] }* @show(%Show* %cvar20)
  %4 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %3, i8* %.unpack11)
  %5 = tail call %Semigroup* @SemigroupString()
  %6 = tail call { i8*, i8, [1 x i8*] }* @plus(%Semigroup* %5)
  %7 = tail call i8* @malloc(i64 5)
  store i8 74, i8* %7, align 8
  %.repack13 = getelementptr inbounds i8, i8* %7, i64 1
  store i8 117, i8* %.repack13, align 1
  %.repack14 = getelementptr inbounds i8, i8* %7, i64 2
  store i8 115, i8* %.repack14, align 2
  %.repack15 = getelementptr inbounds i8, i8* %7, i64 3
  store i8 116, i8* %.repack15, align 1
  %.repack16 = getelementptr inbounds i8, i8* %7, i64 4
  store i8 0, i8* %.repack16, align 4
  %8 = tail call i8* @malloc(i64 16)
  %.repack17 = bitcast i8* %8 to i64*
  store i64 4, i64* %.repack17, align 8
  %.repack18 = getelementptr inbounds i8, i8* %8, i64 8
  %9 = bitcast i8* %.repack18 to i8**
  store i8* %7, i8** %9, align 8
  %10 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %6, i8* %8)
  %11 = bitcast i8* %10 to { i8*, i8, [1 x i8*] }*
  %12 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %11, i8* %4)
  br label %switch_return

branchNothing:                                    ; preds = %entry
  %13 = tail call i8* @malloc(i64 8)
  store i8 110, i8* %13, align 8
  %.repack1 = getelementptr inbounds i8, i8* %13, i64 1
  store i8 111, i8* %.repack1, align 1
  %.repack2 = getelementptr inbounds i8, i8* %13, i64 2
  store i8 116, i8* %.repack2, align 2
  %.repack3 = getelementptr inbounds i8, i8* %13, i64 3
  store i8 104, i8* %.repack3, align 1
  %.repack4 = getelementptr inbounds i8, i8* %13, i64 4
  store i8 105, i8* %.repack4, align 4
  %.repack5 = getelementptr inbounds i8, i8* %13, i64 5
  store i8 110, i8* %.repack5, align 1
  %.repack6 = getelementptr inbounds i8, i8* %13, i64 6
  store i8 103, i8* %.repack6, align 2
  %.repack7 = getelementptr inbounds i8, i8* %13, i64 7
  store i8 0, i8* %.repack7, align 1
  %14 = tail call i8* @malloc(i64 16)
  %.repack = bitcast i8* %14 to i64*
  store i64 7, i64* %.repack, align 8
  %.repack8 = getelementptr inbounds i8, i8* %14, i64 8
  %15 = bitcast i8* %.repack8 to i8**
  store i8* %13, i8** %15, align 8
  br label %switch_return

switch_return:                                    ; preds = %branchNothing, %branchJust
  %.in = phi i8* [ %12, %branchJust ], [ %14, %branchNothing ]
  %16 = bitcast i8* %.in to %String*
  ret %String* %16
}

; Function Attrs: nounwind
define noalias %String* @callClosurelifted22({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) #0 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Bool**
  %2 = load %Bool*, %Bool** %1, align 8
  %3 = tail call %String* @lifted22(%Bool* %2)
  ret %String* %3
}

; Function Attrs: nounwind
define noalias %String* @lifted22(%Bool* nocapture readonly %x1) local_unnamed_addr #0 {
entry:
  %0 = getelementptr %Bool, %Bool* %x1, i64 0, i32 0
  %1 = load i64, i64* %0, align 8
  %switch = icmp eq i64 %1, 1
  br i1 %switch, label %branchFalse, label %branchTrue

branchFalse:                                      ; preds = %entry
  %2 = tail call i8* @malloc(i64 6)
  store i8 70, i8* %2, align 8
  %.repack8 = getelementptr inbounds i8, i8* %2, i64 1
  store i8 97, i8* %.repack8, align 1
  %.repack9 = getelementptr inbounds i8, i8* %2, i64 2
  store i8 108, i8* %.repack9, align 2
  %.repack10 = getelementptr inbounds i8, i8* %2, i64 3
  store i8 115, i8* %.repack10, align 1
  %.repack11 = getelementptr inbounds i8, i8* %2, i64 4
  store i8 101, i8* %.repack11, align 4
  %.repack12 = getelementptr inbounds i8, i8* %2, i64 5
  store i8 0, i8* %.repack12, align 1
  br label %switch_return

branchTrue:                                       ; preds = %entry
  %3 = tail call i8* @malloc(i64 5)
  store i8 84, i8* %3, align 8
  %.repack1 = getelementptr inbounds i8, i8* %3, i64 1
  store i8 114, i8* %.repack1, align 1
  %.repack2 = getelementptr inbounds i8, i8* %3, i64 2
  store i8 117, i8* %.repack2, align 2
  %.repack3 = getelementptr inbounds i8, i8* %3, i64 3
  store i8 101, i8* %.repack3, align 1
  %.repack4 = getelementptr inbounds i8, i8* %3, i64 4
  store i8 0, i8* %.repack4, align 4
  br label %switch_return

switch_return:                                    ; preds = %branchTrue, %branchFalse
  %.sink18 = phi i64 [ 4, %branchTrue ], [ 5, %branchFalse ]
  %.sink = phi i8* [ %3, %branchTrue ], [ %2, %branchFalse ]
  %4 = tail call i8* @malloc(i64 16)
  %.repack = bitcast i8* %4 to i64*
  store i64 %.sink18, i64* %.repack, align 8
  %.repack5 = getelementptr inbounds i8, i8* %4, i64 8
  %5 = bitcast i8* %.repack5 to i8**
  store i8* %.sink, i8** %5, align 8
  %6 = bitcast i8* %4 to %String*
  ret %String* %6
}

define %String* @callClosurelifted23({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = tail call %String* @lifted23(%Int* %2)
  ret %String* %3
}

define %String* @lifted23(%Int* %x1) local_unnamed_addr {
entry:
  %0 = tail call %String* @showInt(%Int* %x1)
  ret %String* %0
}

define %String* @callClosurelifted25({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 0
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %4 = bitcast i8** %3 to %Show**
  %5 = load %Show*, %Show** %4, align 8
  %6 = tail call %String* @lifted25(%Show* %5, %List* %2)
  ret %String* %6
}

define %String* @lifted25(%Show* nocapture readonly %cvar24, %List* %x1) local_unnamed_addr {
entry:
  %0 = tail call { i8*, i8, [1 x i8*] }* @show(%Show* %cvar24)
  %1 = tail call %Functor* @FunctorList()
  %2 = tail call { i8*, i8, [1 x i8*] }* @fmap(%Functor* %1)
  %3 = bitcast { i8*, i8, [1 x i8*] }* %0 to i8*
  %4 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %2, i8* %3)
  %5 = bitcast i8* %4 to { i8*, i8, [1 x i8*] }*
  %6 = bitcast %List* %x1 to i8*
  %7 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %5, i8* %6)
  %8 = bitcast i8* %7 to %List*
  %9 = tail call %Semigroup* @SemigroupString()
  %10 = tail call { i8*, i8, [1 x i8*] }* @plus(%Semigroup* %9)
  %11 = tail call i8* @malloc(i64 2)
  store i8 44, i8* %11, align 8
  %.repack1 = getelementptr inbounds i8, i8* %11, i64 1
  store i8 0, i8* %.repack1, align 1
  %12 = tail call i8* @malloc(i64 16)
  %.repack = bitcast i8* %12 to i64*
  store i64 1, i64* %.repack, align 8
  %.repack2 = getelementptr inbounds i8, i8* %12, i64 8
  %13 = bitcast i8* %.repack2 to i8**
  store i8* %11, i8** %13, align 8
  %14 = tail call %List* @intersperse(i8* %12, %List* %8)
  %15 = tail call %Foldable* @FoldableList()
  %16 = tail call { i8*, i8, [1 x i8*] }* @foldr(%Foldable* %15)
  %17 = tail call i8* @malloc(i64 1)
  store i8 0, i8* %17, align 8
  %18 = tail call i8* @malloc(i64 16)
  %.repack4 = bitcast i8* %18 to i64*
  store i64 0, i64* %.repack4, align 8
  %.repack5 = getelementptr inbounds i8, i8* %18, i64 8
  %19 = bitcast i8* %.repack5 to i8**
  store i8* %17, i8** %19, align 8
  %20 = bitcast { i8*, i8, [1 x i8*] }* %10 to i8*
  %21 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %16, i8* %20)
  %22 = bitcast i8* %21 to { i8*, i8, [1 x i8*] }*
  %23 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %22, i8* %18)
  %24 = bitcast i8* %23 to { i8*, i8, [1 x i8*] }*
  %25 = bitcast %List* %14 to i8*
  %26 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %24, i8* %25)
  %27 = tail call %Semigroup* @SemigroupString()
  %28 = tail call { i8*, i8, [1 x i8*] }* @plus(%Semigroup* %27)
  %29 = tail call i8* @malloc(i64 2)
  store i8 91, i8* %29, align 8
  %.repack8 = getelementptr inbounds i8, i8* %29, i64 1
  store i8 0, i8* %.repack8, align 1
  %30 = tail call i8* @malloc(i64 16)
  %.repack9 = bitcast i8* %30 to i64*
  store i64 1, i64* %.repack9, align 8
  %.repack10 = getelementptr inbounds i8, i8* %30, i64 8
  %31 = bitcast i8* %.repack10 to i8**
  store i8* %29, i8** %31, align 8
  %32 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %28, i8* %30)
  %33 = bitcast i8* %32 to { i8*, i8, [1 x i8*] }*
  %34 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %33, i8* %26)
  %35 = tail call %Semigroup* @SemigroupString()
  %36 = tail call { i8*, i8, [1 x i8*] }* @plus(%Semigroup* %35)
  %37 = tail call i8* @malloc(i64 2)
  store i8 93, i8* %37, align 8
  %.repack13 = getelementptr inbounds i8, i8* %37, i64 1
  store i8 0, i8* %.repack13, align 1
  %38 = tail call i8* @malloc(i64 16)
  %.repack14 = bitcast i8* %38 to i64*
  store i64 1, i64* %.repack14, align 8
  %.repack15 = getelementptr inbounds i8, i8* %38, i64 8
  %39 = bitcast i8* %.repack15 to i8**
  store i8* %37, i8** %39, align 8
  %40 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %36, i8* %34)
  %41 = bitcast i8* %40 to { i8*, i8, [1 x i8*] }*
  %42 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %41, i8* %38)
  %43 = bitcast i8* %42 to %String*
  ret %String* %43
}

; Function Attrs: norecurse nounwind readonly
define i8* @callClosurelifted27({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) #1 {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %1 = load i8*, i8** %0, align 8
  %2 = tail call i8* @lifted27(i8* %1, i8* undef)
  ret i8* %2
}

; Function Attrs: norecurse nounwind readnone
define i8* @lifted27(i8* readnone returned, i8* nocapture readnone %x) local_unnamed_addr #2 {
entry:
  ret i8* %0
}

define %Int* @callClosurelifted28({ i8*, i8, [1 x i8*] }* nocapture readonly %closure) {
entry:
  %0 = getelementptr { i8*, i8, [1 x i8*] }, { i8*, i8, [1 x i8*] }* %closure, i64 0, i32 2, i64 1
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = tail call %Int* @lifted28(%Int* %2, i8* undef)
  ret %Int* %3
}

define %Int* @lifted28(%Int* %x, i8* nocapture readnone %y) local_unnamed_addr {
entry:
  %0 = tail call %Semigroup* @SemigroupInt()
  %1 = tail call { i8*, i8, [1 x i8*] }* @plus(%Semigroup* %0)
  %2 = tail call %Int* @mkInt(i64 1)
  %3 = bitcast %Int* %x to i8*
  %4 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %1, i8* %3)
  %5 = bitcast i8* %4 to { i8*, i8, [1 x i8*] }*
  %6 = bitcast %Int* %2 to i8*
  %7 = tail call i8* @apply1({ i8*, i8, [1 x i8*] }* %5, i8* %6)
  %8 = bitcast i8* %7 to %Int*
  ret %Int* %8
}

define %Bool* @ltInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1
  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1
  %4 = icmp slt i64 %1, %3
  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*
  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8
  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Bool* @gtInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1
  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1
  %4 = icmp sgt i64 %1, %3
  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*
  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8
  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Bool* @eqInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1
  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1
  %4 = icmp eq i64 %1, %3
  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*
  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8
  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Bool* @geqInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1
  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1
  %4 = icmp sge i64 %1, %3
  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*
  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8
  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Bool* @leqInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1
  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1
  %4 = icmp sle i64 %1, %3
  %5 = call i8* @malloc(i64 8)
  %6 = bitcast i8* %5 to i64*
  %7 = sext i1 %4 to i64
  store i64 %7, i64* %6, align 8
  %8 = bitcast i64* %6 to %Bool*
  ret %Bool* %8
}

define %Int* @minusInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1
  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1
  %4 = sub i64 %1, %3
  %5 = call i8* @malloc(i64 16)
  %6 = bitcast i8* %5 to i64*
  %7 = getelementptr i64, i64* %6, i32 1
  store i64 %4, i64* %7, align 8
  %8 = bitcast i64* %6 to %Int*
  ret %Int* %8
}

define %Int* @plusInt(%Int* %a, %Int* %b) {
entry:
  %0 = load %Int, %Int* %a, align 8
  %1 = extractvalue %Int %0, 1
  %2 = load %Int, %Int* %b, align 8
  %3 = extractvalue %Int %2, 1
  %4 = add i64 %1, %3
  %5 = call i8* @malloc(i64 16)
  %6 = bitcast i8* %5 to i64*
  %7 = getelementptr i64, i64* %6, i32 1
  store i64 %4, i64* %7, align 8
  %8 = bitcast i64* %6 to %Int*
  ret %Int* %8
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %String* @showInt(%Int* %x) #3 {
entry:
  %x.addr = alloca %Int*, align 8
  %num_chars = alloca i32, align 4
  %str = alloca %String*, align 8
  store %Int* %x, %Int** %x.addr, align 8
  %0 = call i64 @llvm.objectsize.i64.p0i8(i8* null, i1 false, i1 true)
  %1 = load %Int*, %Int** %x.addr, align 8
  %val = getelementptr inbounds %Int, %Int* %1, i32 0, i32 1
  %2 = load i64, i64* %val, align 8
  %call = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* null, i64 0, i32 0, i64 %0, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i32 0, i32 0), i64 %2)
  %add = add nsw i32 %call, 1
  store i32 %add, i32* %num_chars, align 4
  %3 = load i32, i32* %num_chars, align 4
  %conv = sext i32 %3 to i64
  %call1 = call %String* @mkString(i64 %conv)
  store %String* %call1, %String** %str, align 8
  %4 = load %String*, %String** %str, align 8
  %data = getelementptr inbounds %String, %String* %4, i32 0, i32 1
  %5 = load i8*, i8** %data, align 8
  %6 = load i32, i32* %num_chars, align 4
  %conv2 = sext i32 %6 to i64
  %7 = load %String*, %String** %str, align 8
  %data3 = getelementptr inbounds %String, %String* %7, i32 0, i32 1
  %8 = load i8*, i8** %data3, align 8
  %9 = call i64 @llvm.objectsize.i64.p0i8(i8* %8, i1 false, i1 true)
  %10 = load %Int*, %Int** %x.addr, align 8
  %val4 = getelementptr inbounds %Int, %Int* %10, i32 0, i32 1
  %11 = load i64, i64* %val4, align 8
  %call5 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %5, i64 %conv2, i32 0, i64 %9, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i32 0, i32 0), i64 %11)
  %12 = load %String*, %String** %str, align 8
  ret %String* %12
}

; Function Attrs: nounwind readnone speculatable
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1, i1) #4

declare i32 @__snprintf_chk(i8*, i64, i32, i64, i8*, ...) #5

; Function Attrs: noinline nounwind optnone ssp uwtable
define %String* @mkString(i64 %length) #3 {
entry:
  %length.addr = alloca i64, align 8
  %strPtr = alloca %String*, align 8
  %buffer = alloca i8*, align 8
  store i64 %length, i64* %length.addr, align 8
  %call = call i8* @malloc(i64 16) #6
  %0 = bitcast i8* %call to %String*
  store %String* %0, %String** %strPtr, align 8
  %1 = load i64, i64* %length.addr, align 8
  %add = add i64 %1, 1
  %mul = mul i64 %add, 1
  %call1 = call i8* @malloc(i64 %mul) #6
  store i8* %call1, i8** %buffer, align 8
  %2 = load i64, i64* %length.addr, align 8
  %3 = load %String*, %String** %strPtr, align 8
  %string_length = getelementptr inbounds %String, %String* %3, i32 0, i32 0
  store i64 %2, i64* %string_length, align 8
  %4 = load i8*, i8** %buffer, align 8
  %5 = load %String*, %String** %strPtr, align 8
  %data = getelementptr inbounds %String, %String* %5, i32 0, i32 1
  store i8* %4, i8** %data, align 8
  %6 = load %String*, %String** %strPtr, align 8
  ret %String* %6
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %String* @plusStr(%String* %a, %String* %b) #3 {
entry:
  %a.addr = alloca %String*, align 8
  %b.addr = alloca %String*, align 8
  %num_chars = alloca i32, align 4
  %str = alloca %String*, align 8
  store %String* %a, %String** %a.addr, align 8
  store %String* %b, %String** %b.addr, align 8
  %0 = load %String*, %String** %a.addr, align 8
  %string_length = getelementptr inbounds %String, %String* %0, i32 0, i32 0
  %1 = load i64, i64* %string_length, align 8
  %2 = load %String*, %String** %b.addr, align 8
  %string_length1 = getelementptr inbounds %String, %String* %2, i32 0, i32 0
  %3 = load i64, i64* %string_length1, align 8
  %add = add i64 %1, %3
  %conv = trunc i64 %add to i32
  store i32 %conv, i32* %num_chars, align 4
  %4 = load i32, i32* %num_chars, align 4
  %conv2 = sext i32 %4 to i64
  %call = call %String* @mkString(i64 %conv2)
  store %String* %call, %String** %str, align 8
  %5 = load %String*, %String** %str, align 8
  %data = getelementptr inbounds %String, %String* %5, i32 0, i32 1
  %6 = load i8*, i8** %data, align 8
  %7 = load i32, i32* %num_chars, align 4
  %add3 = add nsw i32 %7, 1
  %conv4 = sext i32 %add3 to i64
  %8 = load %String*, %String** %str, align 8
  %data5 = getelementptr inbounds %String, %String* %8, i32 0, i32 1
  %9 = load i8*, i8** %data5, align 8
  %10 = call i64 @llvm.objectsize.i64.p0i8(i8* %9, i1 false, i1 true)
  %11 = load %String*, %String** %a.addr, align 8
  %data6 = getelementptr inbounds %String, %String* %11, i32 0, i32 1
  %12 = load i8*, i8** %data6, align 8
  %13 = load %String*, %String** %b.addr, align 8
  %data7 = getelementptr inbounds %String, %String* %13, i32 0, i32 1
  %14 = load i8*, i8** %data7, align 8
  %call8 = call i32 (i8*, i64, i32, i64, i8*, ...) @__snprintf_chk(i8* %6, i64 %conv4, i32 0, i64 %10, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.1, i32 0, i32 0), i8* %12, i8* %14)
  %15 = load %String*, %String** %str, align 8
  ret %String* %15
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #3 {
entry:
  %retval = alloca i32, align 4
  %returnVal = alloca %String*, align 8
  store i32 0, i32* %retval, align 4
  %call = call %String* (...) bitcast (%String* ()* @module_main to %String* (...)*)()
  store %String* %call, %String** %returnVal, align 8
  %0 = load %String*, %String** %returnVal, align 8
  %data = getelementptr inbounds %String, %String* %0, i32 0, i32 1
  %1 = load i8*, i8** %data, align 8
  %call1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.2, i32 0, i32 0), i8* %1)
  ret i32 0
}

declare i32 @printf(i8*, ...) #5

attributes #0 = { nounwind }
attributes #1 = { norecurse nounwind readonly }
attributes #2 = { norecurse nounwind readnone }
attributes #3 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind readnone speculatable }
attributes #5 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { allocsize(0) }

!llvm.ident = !{!0}
!llvm.module.flags = !{!1, !2}

!0 = !{!"clang version 5.0.0 (tags/RELEASE_500/final)"}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}

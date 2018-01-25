; ModuleID = 'example'

declare external ccc i8* @malloc(i64)

declare external ccc %Bool* @gtInt(%Int*, %Int*)

%String = type opaque

%Double = type {i64, double}

define external ccc %Double* @mkDouble(double %d){
entry:
  %0 = call ccc i8* @malloc(i64 16)
  %1 = bitcast i8* %0 to %Double*
  %2 = load %Double, %Double* %1, align 8
  %3 = insertvalue %Double %2, double %d, 1
  store %Double %3, %Double* %1, align 8
  ret %Double* %1
}

%Int = type {i64, i64}

define external ccc %Int* @mkInt(i64 %d){
entry:
  %0 = call ccc i8* @malloc(i64 16)
  %1 = bitcast i8* %0 to %Int*
  %2 = load %Int, %Int* %1, align 8
  %3 = insertvalue %Int %2, i64 %d, 1
  store %Int %3, %Int* %1, align 8
  ret %Int* %1
}

define external ccc i8* @apply1({i8*, i8, [1 x i8*]}* %closure, i8* %argPtr){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 1
  %1 = load i8, i8* %argPtr, align 8
  %2 = zext i8 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %"Arity 1", label %default
"Arity 1":
  %4 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 0
  store i8* %argPtr, i8** %4, align 8
  %5 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 0
  %6 = load i8*, i8** %5, align 8
  %7 = bitcast i8* %6 to i8* ({i8*, i8, [1 x i8*]}*)*
  %8 = call ccc i8* %7({i8*, i8, [1 x i8*]}* %closure)
  ret i8* %8
default:
  %9 = sub i32 %2, 1
  %10 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 %9
  store i8* %argPtr, i8** %10, align 8
  %11 = bitcast {i8*, i8, [1 x i8*]}* %closure to i8*
  ret i8* %11
}

%Monad = type {i64}

%Applicative = type {i64}

%Bounded = type {i64}

%Enum = type {i64}

%Foldable = type {i64}

%Ord = type {i64}

%Group = type {i64}

%Monoid = type {i64}

%Semigroup = type {i64}

%Functor = type {i64}

%Show = type {i64}

%Eq = type {i64}

%Bool = type {i64}

%Either = type {i64}

%List = type {i64}

%Maybe = type {i64}

%Ordering = type {i64}

%Tuple = type {i64}

%Unit = type {i64}

%MkMonad = type {i64, %Applicative*, {i8*, i8, [1 x i8*]}*}

define external ccc %Monad* @MkMonad(%Applicative* %a, {i8*, i8, [1 x i8*]}* %a1){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkMonad* getelementptr (%MkMonad, %MkMonad* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkMonad*
  %2 = load %MkMonad, %MkMonad* %1, align 8
  %3 = insertvalue %MkMonad %2, i64 0, 0
  %4 = insertvalue %MkMonad %2, %Applicative* %a, 1
  %5 = insertvalue %MkMonad %4, {i8*, i8, [1 x i8*]}* %a1, 2
  store %MkMonad %5, %MkMonad* %1, align 8
  %6 = bitcast %MkMonad* %1 to %Monad*
  ret %Monad* %6
}

%MkApplicative = type {i64, %Functor*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*}

define external ccc %Applicative* @MkApplicative(%Functor* %a, {i8*, i8, [1 x i8*]}* %a1, {i8*, i8, [1 x i8*]}* %a2){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkApplicative* getelementptr (%MkApplicative, %MkApplicative* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkApplicative*
  %2 = load %MkApplicative, %MkApplicative* %1, align 8
  %3 = insertvalue %MkApplicative %2, i64 0, 0
  %4 = insertvalue %MkApplicative %2, %Functor* %a, 1
  %5 = insertvalue %MkApplicative %4, {i8*, i8, [1 x i8*]}* %a1, 2
  %6 = insertvalue %MkApplicative %5, {i8*, i8, [1 x i8*]}* %a2, 3
  store %MkApplicative %6, %MkApplicative* %1, align 8
  %7 = bitcast %MkApplicative* %1 to %Applicative*
  ret %Applicative* %7
}

%MkBounded = type {i64, i8*, i8*}

define external ccc %Bounded* @MkBounded(i8* %a, i8* %a1){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkBounded* getelementptr (%MkBounded, %MkBounded* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkBounded*
  %2 = load %MkBounded, %MkBounded* %1, align 8
  %3 = insertvalue %MkBounded %2, i64 0, 0
  %4 = insertvalue %MkBounded %2, i8* %a, 1
  %5 = insertvalue %MkBounded %4, i8* %a1, 2
  store %MkBounded %5, %MkBounded* %1, align 8
  %6 = bitcast %MkBounded* %1 to %Bounded*
  ret %Bounded* %6
}

%MkEnum = type {i64, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*}

define external ccc %Enum* @MkEnum({i8*, i8, [1 x i8*]}* %a, {i8*, i8, [1 x i8*]}* %a1, {i8*, i8, [1 x i8*]}* %a2, {i8*, i8, [1 x i8*]}* %a3){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkEnum* getelementptr (%MkEnum, %MkEnum* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkEnum*
  %2 = load %MkEnum, %MkEnum* %1, align 8
  %3 = insertvalue %MkEnum %2, i64 0, 0
  %4 = insertvalue %MkEnum %2, {i8*, i8, [1 x i8*]}* %a, 1
  %5 = insertvalue %MkEnum %4, {i8*, i8, [1 x i8*]}* %a1, 2
  %6 = insertvalue %MkEnum %5, {i8*, i8, [1 x i8*]}* %a2, 3
  %7 = insertvalue %MkEnum %6, {i8*, i8, [1 x i8*]}* %a3, 4
  store %MkEnum %7, %MkEnum* %1, align 8
  %8 = bitcast %MkEnum* %1 to %Enum*
  ret %Enum* %8
}

%MkFoldable = type {i64, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*}

define external ccc %Foldable* @MkFoldable({i8*, i8, [1 x i8*]}* %a, {i8*, i8, [1 x i8*]}* %a1){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkFoldable* getelementptr (%MkFoldable, %MkFoldable* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkFoldable*
  %2 = load %MkFoldable, %MkFoldable* %1, align 8
  %3 = insertvalue %MkFoldable %2, i64 0, 0
  %4 = insertvalue %MkFoldable %2, {i8*, i8, [1 x i8*]}* %a, 1
  %5 = insertvalue %MkFoldable %4, {i8*, i8, [1 x i8*]}* %a1, 2
  store %MkFoldable %5, %MkFoldable* %1, align 8
  %6 = bitcast %MkFoldable* %1 to %Foldable*
  ret %Foldable* %6
}

%MkOrd = type {i64, %Eq*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}*}

define external ccc %Ord* @MkOrd(%Eq* %a, {i8*, i8, [1 x i8*]}* %a1, {i8*, i8, [1 x i8*]}* %a2, {i8*, i8, [1 x i8*]}* %a3, {i8*, i8, [1 x i8*]}* %a4, {i8*, i8, [1 x i8*]}* %a5, {i8*, i8, [1 x i8*]}* %a6, {i8*, i8, [1 x i8*]}* %a7){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkOrd* getelementptr (%MkOrd, %MkOrd* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkOrd*
  %2 = load %MkOrd, %MkOrd* %1, align 8
  %3 = insertvalue %MkOrd %2, i64 0, 0
  %4 = insertvalue %MkOrd %2, %Eq* %a, 1
  %5 = insertvalue %MkOrd %4, {i8*, i8, [1 x i8*]}* %a1, 2
  %6 = insertvalue %MkOrd %5, {i8*, i8, [1 x i8*]}* %a2, 3
  %7 = insertvalue %MkOrd %6, {i8*, i8, [1 x i8*]}* %a3, 4
  %8 = insertvalue %MkOrd %7, {i8*, i8, [1 x i8*]}* %a4, 5
  %9 = insertvalue %MkOrd %8, {i8*, i8, [1 x i8*]}* %a5, 6
  %10 = insertvalue %MkOrd %9, {i8*, i8, [1 x i8*]}* %a6, 7
  %11 = insertvalue %MkOrd %10, {i8*, i8, [1 x i8*]}* %a7, 8
  store %MkOrd %11, %MkOrd* %1, align 8
  %12 = bitcast %MkOrd* %1 to %Ord*
  ret %Ord* %12
}

%MkGroup = type {i64, %Monoid*, {i8*, i8, [1 x i8*]}*}

define external ccc %Group* @MkGroup(%Monoid* %a, {i8*, i8, [1 x i8*]}* %a1){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkGroup* getelementptr (%MkGroup, %MkGroup* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkGroup*
  %2 = load %MkGroup, %MkGroup* %1, align 8
  %3 = insertvalue %MkGroup %2, i64 0, 0
  %4 = insertvalue %MkGroup %2, %Monoid* %a, 1
  %5 = insertvalue %MkGroup %4, {i8*, i8, [1 x i8*]}* %a1, 2
  store %MkGroup %5, %MkGroup* %1, align 8
  %6 = bitcast %MkGroup* %1 to %Group*
  ret %Group* %6
}

%MkMonoid = type {i64, %Semigroup*, i8*}

define external ccc %Monoid* @MkMonoid(%Semigroup* %a, i8* %a1){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkMonoid* getelementptr (%MkMonoid, %MkMonoid* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkMonoid*
  %2 = load %MkMonoid, %MkMonoid* %1, align 8
  %3 = insertvalue %MkMonoid %2, i64 0, 0
  %4 = insertvalue %MkMonoid %2, %Semigroup* %a, 1
  %5 = insertvalue %MkMonoid %4, i8* %a1, 2
  store %MkMonoid %5, %MkMonoid* %1, align 8
  %6 = bitcast %MkMonoid* %1 to %Monoid*
  ret %Monoid* %6
}

%MkSemigroup = type {i64, {i8*, i8, [1 x i8*]}*}

define external ccc %Semigroup* @MkSemigroup({i8*, i8, [1 x i8*]}* %a){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkSemigroup* getelementptr (%MkSemigroup, %MkSemigroup* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkSemigroup*
  %2 = load %MkSemigroup, %MkSemigroup* %1, align 8
  %3 = insertvalue %MkSemigroup %2, i64 0, 0
  %4 = insertvalue %MkSemigroup %2, {i8*, i8, [1 x i8*]}* %a, 1
  store %MkSemigroup %4, %MkSemigroup* %1, align 8
  %5 = bitcast %MkSemigroup* %1 to %Semigroup*
  ret %Semigroup* %5
}

%MkFunctor = type {i64, {i8*, i8, [1 x i8*]}*}

define external ccc %Functor* @MkFunctor({i8*, i8, [1 x i8*]}* %a){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkFunctor* getelementptr (%MkFunctor, %MkFunctor* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkFunctor*
  %2 = load %MkFunctor, %MkFunctor* %1, align 8
  %3 = insertvalue %MkFunctor %2, i64 0, 0
  %4 = insertvalue %MkFunctor %2, {i8*, i8, [1 x i8*]}* %a, 1
  store %MkFunctor %4, %MkFunctor* %1, align 8
  %5 = bitcast %MkFunctor* %1 to %Functor*
  ret %Functor* %5
}

%MkShow = type {i64, {i8*, i8, [1 x i8*]}*}

define external ccc %Show* @MkShow({i8*, i8, [1 x i8*]}* %a){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkShow* getelementptr (%MkShow, %MkShow* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkShow*
  %2 = load %MkShow, %MkShow* %1, align 8
  %3 = insertvalue %MkShow %2, i64 0, 0
  %4 = insertvalue %MkShow %2, {i8*, i8, [1 x i8*]}* %a, 1
  store %MkShow %4, %MkShow* %1, align 8
  %5 = bitcast %MkShow* %1 to %Show*
  ret %Show* %5
}

%MkEq = type {i64, {i8*, i8, [1 x i8*]}*}

define external ccc %Eq* @MkEq({i8*, i8, [1 x i8*]}* %a){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkEq* getelementptr (%MkEq, %MkEq* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkEq*
  %2 = load %MkEq, %MkEq* %1, align 8
  %3 = insertvalue %MkEq %2, i64 0, 0
  %4 = insertvalue %MkEq %2, {i8*, i8, [1 x i8*]}* %a, 1
  store %MkEq %4, %MkEq* %1, align 8
  %5 = bitcast %MkEq* %1 to %Eq*
  ret %Eq* %5
}

%True = type {i64}

define external ccc %Bool* @True(){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%True* getelementptr (%True, %True* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %True*
  %2 = load %True, %True* %1, align 8
  %3 = insertvalue %True %2, i64 0, 0
  store %True %2, %True* %1, align 8
  %4 = bitcast %True* %1 to %Bool*
  ret %Bool* %4
}

%False = type {i64}

define external ccc %Bool* @False(){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%False* getelementptr (%False, %False* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %False*
  %2 = load %False, %False* %1, align 8
  %3 = insertvalue %False %2, i64 0, 0
  store %False %2, %False* %1, align 8
  %4 = bitcast %False* %1 to %Bool*
  ret %Bool* %4
}

%Left = type {i64, i8*}

define external ccc %Either* @Left(i8* %a){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%Left* getelementptr (%Left, %Left* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %Left*
  %2 = load %Left, %Left* %1, align 8
  %3 = insertvalue %Left %2, i64 0, 0
  %4 = insertvalue %Left %2, i8* %a, 1
  store %Left %4, %Left* %1, align 8
  %5 = bitcast %Left* %1 to %Either*
  ret %Either* %5
}

%Right = type {i64, i8*}

define external ccc %Either* @Right(i8* %a){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%Right* getelementptr (%Right, %Right* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %Right*
  %2 = load %Right, %Right* %1, align 8
  %3 = insertvalue %Right %2, i64 0, 0
  %4 = insertvalue %Right %2, i8* %a, 1
  store %Right %4, %Right* %1, align 8
  %5 = bitcast %Right* %1 to %Either*
  ret %Either* %5
}

%Cons = type {i64, i8*, %List*}

define external ccc %List* @Cons(i8* %a, %List* %a1){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%Cons* getelementptr (%Cons, %Cons* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %Cons*
  %2 = load %Cons, %Cons* %1, align 8
  %3 = insertvalue %Cons %2, i64 0, 0
  %4 = insertvalue %Cons %2, i8* %a, 1
  %5 = insertvalue %Cons %4, %List* %a1, 2
  store %Cons %5, %Cons* %1, align 8
  %6 = bitcast %Cons* %1 to %List*
  ret %List* %6
}

%Nil = type {i64}

define external ccc %List* @Nil(){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%Nil* getelementptr (%Nil, %Nil* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %Nil*
  %2 = load %Nil, %Nil* %1, align 8
  %3 = insertvalue %Nil %2, i64 0, 0
  store %Nil %2, %Nil* %1, align 8
  %4 = bitcast %Nil* %1 to %List*
  ret %List* %4
}

%Just = type {i64, i8*}

define external ccc %Maybe* @Just(i8* %a){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%Just* getelementptr (%Just, %Just* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %Just*
  %2 = load %Just, %Just* %1, align 8
  %3 = insertvalue %Just %2, i64 0, 0
  %4 = insertvalue %Just %2, i8* %a, 1
  store %Just %4, %Just* %1, align 8
  %5 = bitcast %Just* %1 to %Maybe*
  ret %Maybe* %5
}

%Nothing = type {i64}

define external ccc %Maybe* @Nothing(){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%Nothing* getelementptr (%Nothing, %Nothing* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %Nothing*
  %2 = load %Nothing, %Nothing* %1, align 8
  %3 = insertvalue %Nothing %2, i64 0, 0
  store %Nothing %2, %Nothing* %1, align 8
  %4 = bitcast %Nothing* %1 to %Maybe*
  ret %Maybe* %4
}

%LE = type {i64}

define external ccc %Ordering* @LE(){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%LE* getelementptr (%LE, %LE* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %LE*
  %2 = load %LE, %LE* %1, align 8
  %3 = insertvalue %LE %2, i64 0, 0
  store %LE %2, %LE* %1, align 8
  %4 = bitcast %LE* %1 to %Ordering*
  ret %Ordering* %4
}

%GE = type {i64}

define external ccc %Ordering* @GE(){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%GE* getelementptr (%GE, %GE* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %GE*
  %2 = load %GE, %GE* %1, align 8
  %3 = insertvalue %GE %2, i64 0, 0
  store %GE %2, %GE* %1, align 8
  %4 = bitcast %GE* %1 to %Ordering*
  ret %Ordering* %4
}

%EQ = type {i64}

define external ccc %Ordering* @EQ(){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%EQ* getelementptr (%EQ, %EQ* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %EQ*
  %2 = load %EQ, %EQ* %1, align 8
  %3 = insertvalue %EQ %2, i64 0, 0
  store %EQ %2, %EQ* %1, align 8
  %4 = bitcast %EQ* %1 to %Ordering*
  ret %Ordering* %4
}

%T = type {i64, i8*, i8*}

define external ccc %Tuple* @T(i8* %a, i8* %a1){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%T* getelementptr (%T, %T* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %T*
  %2 = load %T, %T* %1, align 8
  %3 = insertvalue %T %2, i64 0, 0
  %4 = insertvalue %T %2, i8* %a, 1
  %5 = insertvalue %T %4, i8* %a1, 2
  store %T %5, %T* %1, align 8
  %6 = bitcast %T* %1 to %Tuple*
  ret %Tuple* %6
}

%MkUnit = type {i64}

define external ccc %Unit* @MkUnit(){
entryC:
  %0 = call ccc i8* @malloc(i64 ptrtoint (%MkUnit* getelementptr (%MkUnit, %MkUnit* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to %MkUnit*
  %2 = load %MkUnit, %MkUnit* %1, align 8
  %3 = insertvalue %MkUnit %2, i64 0, 0
  store %MkUnit %2, %MkUnit* %1, align 8
  %4 = bitcast %MkUnit* %1 to %Unit*
  ret %Unit* %4
}

define external ccc %Functor* @FunctorEither_a(){
entry:
  %0 = call ccc i8* @malloc(i64 ptrtoint ({i8*, i8, [1 x i8*]}* getelementptr ({i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to {i8*, i8, [1 x i8*]}*
  %2 = load {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %1, align 8
  %3 = bitcast %Either* ({i8*, i8, [1 x i8*]}*)* @callClosurelifted7 to i8*
  %4 = insertvalue {i8*, i8, [1 x i8*]} %2, i8* %3, 0
  %5 = insertvalue {i8*, i8, [1 x i8*]} %4, i8 2, 1
  store {i8*, i8, [1 x i8*]} %5, {i8*, i8, [1 x i8*]}* %1, align 8
  %6 = call ccc %Functor* @MkFunctor({i8*, i8, [1 x i8*]}* %1)
  ret %Functor* %6
}

define external ccc %Semigroup* @SemigroupList_a(){
entry:
  %0 = call ccc i8* @malloc(i64 ptrtoint ({i8*, i8, [1 x i8*]}* getelementptr ({i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to {i8*, i8, [1 x i8*]}*
  %2 = load {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %1, align 8
  %3 = bitcast %List* ({i8*, i8, [1 x i8*]}*)* @callClosurelifted10 to i8*
  %4 = insertvalue {i8*, i8, [1 x i8*]} %2, i8* %3, 0
  %5 = insertvalue {i8*, i8, [1 x i8*]} %4, i8 2, 1
  store {i8*, i8, [1 x i8*]} %5, {i8*, i8, [1 x i8*]}* %1, align 8
  %6 = call ccc %Semigroup* @MkSemigroup({i8*, i8, [1 x i8*]}* %1)
  ret %Semigroup* %6
}

define external ccc %Semigroup* @SemigroupMaybe_a(){
entry:
  %0 = call ccc i8* @malloc(i64 ptrtoint ({i8*, i8, [1 x i8*]}* getelementptr ({i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to {i8*, i8, [1 x i8*]}*
  %2 = load {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %1, align 8
  %3 = bitcast %Maybe* ({i8*, i8, [1 x i8*]}*)* @callClosurelifted11 to i8*
  %4 = insertvalue {i8*, i8, [1 x i8*]} %2, i8* %3, 0
  %5 = insertvalue {i8*, i8, [1 x i8*]} %4, i8 2, 1
  store {i8*, i8, [1 x i8*]} %5, {i8*, i8, [1 x i8*]}* %1, align 8
  %6 = call ccc %Semigroup* @MkSemigroup({i8*, i8, [1 x i8*]}* %1)
  ret %Semigroup* %6
}

define external ccc %Show* @ShowMaybe_a(%Show* %x1){
entry:
  %0 = call ccc i8* @malloc(i64 ptrtoint ({i8*, i8, [1 x i8*]}* getelementptr ({i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to {i8*, i8, [1 x i8*]}*
  %2 = load {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %1, align 8
  %3 = bitcast %String* ({i8*, i8, [1 x i8*]}*)* @callClosurelifted15 to i8*
  %4 = insertvalue {i8*, i8, [1 x i8*]} %2, i8* %3, 0
  %5 = insertvalue {i8*, i8, [1 x i8*]} %4, i8 2, 1
  store {i8*, i8, [1 x i8*]} %5, {i8*, i8, [1 x i8*]}* %1, align 8
  %6 = bitcast %Show* %x1 to i8*
  %7 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %1, i8* %6)
  %8 = bitcast i8* %7 to {i8*, i8, [1 x i8*]}*
  %9 = call ccc %Show* @MkShow({i8*, i8, [1 x i8*]}* %8)
  ret %Show* %9
}

define external ccc %Show* @ShowList_a(%Show* %x1){
entry:
  %0 = call ccc i8* @malloc(i64 ptrtoint ({i8*, i8, [1 x i8*]}* getelementptr ({i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to {i8*, i8, [1 x i8*]}*
  %2 = load {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %1, align 8
  %3 = bitcast %String* ({i8*, i8, [1 x i8*]}*)* @callClosurelifted19 to i8*
  %4 = insertvalue {i8*, i8, [1 x i8*]} %2, i8* %3, 0
  %5 = insertvalue {i8*, i8, [1 x i8*]} %4, i8 2, 1
  store {i8*, i8, [1 x i8*]} %5, {i8*, i8, [1 x i8*]}* %1, align 8
  %6 = bitcast %Show* %x1 to i8*
  %7 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %1, i8* %6)
  %8 = bitcast i8* %7 to {i8*, i8, [1 x i8*]}*
  %9 = call ccc %Show* @MkShow({i8*, i8, [1 x i8*]}* %8)
  ret %Show* %9
}

define external ccc {i8*, i8, [1 x i8*]}* @null(%Foldable* %x1){
entry:
  %0 = call ccc %Bool* @False()
  %1 = bitcast %Bool* %0 to i8*
  %2 = call ccc {i8*, i8, [1 x i8*]}* @const(i8* %1)
  %3 = bitcast {i8*, i8, [1 x i8*]}* %2 to i8*
  %4 = call ccc {i8*, i8, [1 x i8*]}* @const(i8* %3)
  %5 = call ccc {i8*, i8, [1 x i8*]}* @foldr(%Foldable* %x1)
  %6 = call ccc %Bool* @True()
  %7 = bitcast {i8*, i8, [1 x i8*]}* %4 to i8*
  %8 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %5, i8* %7)
  %9 = bitcast i8* %8 to {i8*, i8, [1 x i8*]}*
  %10 = bitcast %Bool* %6 to i8*
  %11 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %9, i8* %10)
  %12 = bitcast i8* %11 to {i8*, i8, [1 x i8*]}*
  ret {i8*, i8, [1 x i8*]}* %12
}

define external ccc {i8*, i8, [1 x i8*]}* @const(i8* %x1){
entry:
  %0 = call ccc i8* @malloc(i64 ptrtoint ({i8*, i8, [1 x i8*]}* getelementptr ({i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* zeroinitializer, i32 1) to i64))
  %1 = bitcast i8* %0 to {i8*, i8, [1 x i8*]}*
  %2 = load {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %1, align 8
  %3 = bitcast i8* ({i8*, i8, [1 x i8*]}*)* @callClosurelifted21 to i8*
  %4 = insertvalue {i8*, i8, [1 x i8*]} %2, i8* %3, 0
  %5 = insertvalue {i8*, i8, [1 x i8*]} %4, i8 2, 1
  store {i8*, i8, [1 x i8*]} %5, {i8*, i8, [1 x i8*]}* %1, align 8
  %6 = bitcast i8* %x1 to i8*
  %7 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %1, i8* %6)
  %8 = bitcast i8* %7 to {i8*, i8, [1 x i8*]}*
  ret {i8*, i8, [1 x i8*]}* %8
}

define external ccc %Maybe* @head(%List* %x1){
entry:
  %0 = getelementptr %List, %List* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List* %x1 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = call ccc %Maybe* @Just(i8* %4)
  br label %switch_return
branch1:
  %7 = bitcast %List* %x1 to %Nil*
  %8 = load %Nil, %Nil* %7, align 8
  %9 = call ccc %Maybe* @Nothing()
  br label %switch_return
trivial_branch:
  %10 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %11 = phi %Maybe* [%6, %branch], [%9, %branch1]
  ret %Maybe* %11
}

define external ccc %Maybe* @init(%List* %x1){
entry:
  %0 = getelementptr %List, %List* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch2 [i64 1, label %branch i64 2, label %branch4]
branch:
  %2 = bitcast %List* %x1 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = getelementptr %List, %List* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  switch i64 %7, label %trivial_branch [i64 1, label %branch1]
branch1:
  %8 = bitcast %List* %5 to %Nil*
  %9 = load %Nil, %Nil* %8, align 8
  %10 = call ccc %List* @Nil()
  %11 = bitcast %List* %10 to i8*
  %12 = call ccc %Maybe* @Just(i8* %11)
  br label %switch_return1
trivial_branch:
  %13 = call ccc %Maybe* @init(%List* %5)
  %14 = getelementptr %Maybe, %Maybe* %13, i32 0, i32 0
  %15 = load i64, i64* %14, align 8
  switch i64 %15, label %trivial_branch1 [i64 1, label %branch2 i64 2, label %branch3]
branch2:
  %16 = bitcast %Maybe* %13 to %Just*
  %17 = load %Just, %Just* %16, align 8
  %18 = extractvalue %Just %17, 1
  %19 = bitcast i8* %18 to %List*
  %20 = call ccc %List* @Cons(i8* %4, %List* %19)
  %21 = bitcast %List* %20 to i8*
  %22 = call ccc %Maybe* @Just(i8* %21)
  br label %switch_return
branch3:
  %23 = bitcast %Maybe* %13 to %Nothing*
  %24 = load %Nothing, %Nothing* %23, align 8
  %25 = call ccc %List* @Nil()
  %26 = call ccc %List* @Cons(i8* %4, %List* %25)
  %27 = bitcast %List* %26 to i8*
  %28 = call ccc %Maybe* @Just(i8* %27)
  br label %switch_return
trivial_branch1:
  %29 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %30 = phi %Maybe* [%22, %branch2], [%28, %branch3]
  br label %switch_return1
switch_return1:
  %31 = phi %Maybe* [%12, %branch1]
  br label %switch_return2
branch4:
  %32 = bitcast %List* %x1 to %Nil*
  %33 = load %Nil, %Nil* %32, align 8
  %34 = call ccc %Maybe* @Nothing()
  br label %switch_return2
trivial_branch2:
  %35 = call ccc i8* @failedPattern()
  br label %switch_return2
switch_return2:
  %36 = phi %Maybe* [%31, %branch], [%34, %branch4]
  ret %Maybe* %36
}

define external ccc %List* @callClosureintersperse({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to i8**
  %5 = load i8*, i8** %4, align 8
  %6 = call ccc %List* @intersperse(i8* %5, %List* %2)
  ret %List* %6
}

define external ccc %List* @intersperse(i8* %x1, %List* %x2){
entry:
  %0 = getelementptr %List, %List* %x2, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch1 [i64 1, label %branch i64 2, label %branch2]
branch:
  %2 = bitcast %List* %x2 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = getelementptr %List, %List* %5, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  switch i64 %7, label %trivial_branch [i64 1, label %branch1]
branch1:
  %8 = bitcast %List* %5 to %Nil*
  %9 = load %Nil, %Nil* %8, align 8
  %10 = call ccc %List* @Nil()
  %11 = call ccc %List* @Cons(i8* %4, %List* %10)
  br label %switch_return
trivial_branch:
  %12 = call ccc %List* @intersperse(i8* %x1, %List* %5)
  %13 = call ccc %List* @Cons(i8* %x1, %List* %12)
  %14 = call ccc %List* @Cons(i8* %4, %List* %13)
  br label %switch_return
switch_return:
  %15 = phi %List* [%11, %branch1]
  br label %switch_return1
branch2:
  %16 = bitcast %List* %x2 to %Nil*
  %17 = load %Nil, %Nil* %16, align 8
  %18 = call ccc %List* @Nil()
  br label %switch_return1
trivial_branch1:
  %19 = call ccc i8* @failedPattern()
  br label %switch_return1
switch_return1:
  %20 = phi %List* [%15, %branch], [%18, %branch2]
  ret %List* %20
}

define external ccc %Maybe* @last(%List* %x1){
entry:
  %0 = getelementptr %List, %List* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List* %x1 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = call ccc %Maybe* @Just(i8* %4)
  br label %switch_return
branch1:
  %7 = bitcast %List* %x1 to %Nil*
  %8 = load %Nil, %Nil* %7, align 8
  %9 = call ccc %Maybe* @Nothing()
  br label %switch_return
trivial_branch:
  %10 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %11 = phi %Maybe* [%6, %branch], [%9, %branch1]
  ret %Maybe* %11
}

define external ccc %Int* @callClosurelength({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to i8**
  %2 = load i8*, i8** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Foldable**
  %5 = load %Foldable*, %Foldable** %4, align 8
  %6 = call ccc %Int* @length(%Foldable* %5, i8* %2)
  ret %Int* %6
}

; define external ccc %Int* @length(%Foldable* %x1, i8* %x2){
; entry:
;   %0 = call ccc {i8*, i8, [1 x i8*]}* @foldl(%Foldable* %x1)
;   %1 = call ccc i8* @malloc(i64 ptrtoint ({i8*, i8, [1 x i8*]}* getelementptr ({i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* zeroinitializer, i32 1) to i64))
;   %2 = bitcast i8* %1 to {i8*, i8, [1 x i8*]}*
;   %3 = load {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %2, align 8
;   %4 = bitcast %Int* ({i8*, i8, [1 x i8*]}*)* @callClosurelifted22 to i8*
;   %5 = insertvalue {i8*, i8, [1 x i8*]} %3, i8* %4, 0
;   %6 = insertvalue {i8*, i8, [1 x i8*]} %5, i8 2, 1
;   store {i8*, i8, [1 x i8*]} %6, {i8*, i8, [1 x i8*]}* %2, align 8
;   %7 = call ccc %Int* @mkInt(i64 0)
;   %8 = bitcast {i8*, i8, [1 x i8*]}* %2 to i8*
;   %9 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %0, i8* %8)
;   %10 = bitcast i8* %9 to {i8*, i8, [1 x i8*]}*
;   %11 = bitcast %Int* %7 to i8*
;   %12 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %10, i8* %11)
;   %13 = bitcast i8* %12 to {i8*, i8, [1 x i8*]}*
;   %14 = bitcast i8* %x2 to i8*
;   %15 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %13, i8* %14)
;   %16 = bitcast i8* %15 to {i8*, i8, [1 x i8*]}*
;   ret {i8*, i8, [1 x i8*]}* %16
; }

define external ccc %Maybe* @tail(%List* %x1){
entry:
  %0 = getelementptr %List, %List* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List* %x1 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = bitcast %List* %5 to i8*
  %7 = call ccc %Maybe* @Just(i8* %6)
  br label %switch_return
branch1:
  %8 = bitcast %List* %x1 to %Nil*
  %9 = load %Nil, %Nil* %8, align 8
  %10 = call ccc %Maybe* @Nothing()
  br label %switch_return
trivial_branch:
  %11 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %12 = phi %Maybe* [%7, %branch], [%10, %branch1]
  ret %Maybe* %12
}

define external ccc %Maybe* @uncons(%List* %x1){
entry:
  %0 = getelementptr %List, %List* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List* %x1 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = bitcast %List* %5 to i8*
  %7 = call ccc %Tuple* @T(i8* %4, i8* %6)
  %8 = bitcast %Tuple* %7 to i8*
  %9 = call ccc %Maybe* @Just(i8* %8)
  br label %switch_return
branch1:
  %10 = bitcast %List* %x1 to %Nil*
  %11 = load %Nil, %Nil* %10, align 8
  %12 = call ccc %Maybe* @Nothing()
  br label %switch_return
trivial_branch:
  %13 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %14 = phi %Maybe* [%9, %branch], [%12, %branch1]
  ret %Maybe* %14
}

define external ccc %List* @callClosurezipWith({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %List**
  %5 = load %List*, %List** %4, align 8
  %6 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 3
  %7 = bitcast i8** %6 to {i8*, i8, [1 x i8*]}**
  %8 = load {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}** %7, align 8
  %9 = call ccc %List* @zipWith({i8*, i8, [1 x i8*]}* %8, %List* %5, %List* %2)
  ret %List* %9
}

define external ccc %List* @zipWith({i8*, i8, [1 x i8*]}* %x1, %List* %x2, %List* %x3){
entry:
  %0 = getelementptr %List, %List* %x2, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch1 [i64 1, label %branch]
branch:
  %2 = bitcast %List* %x2 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = getelementptr %List, %List* %x3, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  switch i64 %7, label %trivial_branch [i64 1, label %branch1]
branch1:
  %8 = bitcast %List* %x3 to %Cons*
  %9 = load %Cons, %Cons* %8, align 8
  %10 = extractvalue %Cons %9, 1
  %11 = extractvalue %Cons %9, 2
  %12 = bitcast i8* %4 to i8*
  %13 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %x1, i8* %12)
  %14 = bitcast i8* %13 to {i8*, i8, [1 x i8*]}*
  %15 = bitcast i8* %10 to i8*
  %16 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %14, i8* %15)
  %17 = bitcast i8* %16 to i8*
  %18 = call ccc %List* @zipWith({i8*, i8, [1 x i8*]}* %x1, %List* %5, %List* %11)
  %19 = call ccc %List* @Cons(i8* %17, %List* %18)
  br label %switch_return
trivial_branch:
  %20 = call ccc %List* @Nil()
  br label %switch_return
switch_return:
  %21 = phi %List* [%19, %branch1]
  br label %switch_return1
trivial_branch1:
  %22 = call ccc %List* @Nil()
  br label %switch_return1
switch_return1:
  %23 = phi %List* [%21, %branch]
  ret %List* %23
}

define external ccc {i8*, i8, [1 x i8*]}* @bind(%Monad* %x1){
entry:
  %0 = getelementptr %Monad, %Monad* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Monad* %x1 to %MkMonad*
  %3 = load %MkMonad, %MkMonad* %2, align 8
  %4 = extractvalue %MkMonad %3, 2
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @pure(%Applicative* %x1){
entry:
  %0 = getelementptr %Applicative, %Applicative* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Applicative* %x1 to %MkApplicative*
  %3 = load %MkApplicative, %MkApplicative* %2, align 8
  %4 = extractvalue %MkApplicative %3, 3
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @ap(%Applicative* %x1){
entry:
  %0 = getelementptr %Applicative, %Applicative* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Applicative* %x1 to %MkApplicative*
  %3 = load %MkApplicative, %MkApplicative* %2, align 8
  %4 = extractvalue %MkApplicative %3, 2
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc i8* @minBound(%Bounded* %x1){
entry:
  %0 = getelementptr %Bounded, %Bounded* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Bounded* %x1 to %MkBounded*
  %3 = load %MkBounded, %MkBounded* %2, align 8
  %4 = extractvalue %MkBounded %3, 2
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi i8* [%4, %branch]
  ret i8* %6
}

define external ccc i8* @maxBound(%Bounded* %x1){
entry:
  %0 = getelementptr %Bounded, %Bounded* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Bounded* %x1 to %MkBounded*
  %3 = load %MkBounded, %MkBounded* %2, align 8
  %4 = extractvalue %MkBounded %3, 1
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi i8* [%4, %branch]
  ret i8* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @toEnum(%Enum* %x1){
entry:
  %0 = getelementptr %Enum, %Enum* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Enum* %x1 to %MkEnum*
  %3 = load %MkEnum, %MkEnum* %2, align 8
  %4 = extractvalue %MkEnum %3, 4
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @succ(%Enum* %x1){
entry:
  %0 = getelementptr %Enum, %Enum* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Enum* %x1 to %MkEnum*
  %3 = load %MkEnum, %MkEnum* %2, align 8
  %4 = extractvalue %MkEnum %3, 3
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @pred(%Enum* %x1){
entry:
  %0 = getelementptr %Enum, %Enum* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Enum* %x1 to %MkEnum*
  %3 = load %MkEnum, %MkEnum* %2, align 8
  %4 = extractvalue %MkEnum %3, 2
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @fromEnum(%Enum* %x1){
entry:
  %0 = getelementptr %Enum, %Enum* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Enum* %x1 to %MkEnum*
  %3 = load %MkEnum, %MkEnum* %2, align 8
  %4 = extractvalue %MkEnum %3, 1
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @foldr(%Foldable* %x1){
entry:
  %0 = getelementptr %Foldable, %Foldable* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Foldable* %x1 to %MkFoldable*
  %3 = load %MkFoldable, %MkFoldable* %2, align 8
  %4 = extractvalue %MkFoldable %3, 2
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @foldl(%Foldable* %x1){
entry:
  %0 = getelementptr %Foldable, %Foldable* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Foldable* %x1 to %MkFoldable*
  %3 = load %MkFoldable, %MkFoldable* %2, align 8
  %4 = extractvalue %MkFoldable %3, 1
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @min(%Ord* %x1){
entry:
  %0 = getelementptr %Ord, %Ord* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord* %x1 to %MkOrd*
  %3 = load %MkOrd, %MkOrd* %2, align 8
  %4 = extractvalue %MkOrd %3, 8
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @max(%Ord* %x1){
entry:
  %0 = getelementptr %Ord, %Ord* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord* %x1 to %MkOrd*
  %3 = load %MkOrd, %MkOrd* %2, align 8
  %4 = extractvalue %MkOrd %3, 7
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @lt(%Ord* %x1){
entry:
  %0 = getelementptr %Ord, %Ord* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord* %x1 to %MkOrd*
  %3 = load %MkOrd, %MkOrd* %2, align 8
  %4 = extractvalue %MkOrd %3, 6
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @leq(%Ord* %x1){
entry:
  %0 = getelementptr %Ord, %Ord* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord* %x1 to %MkOrd*
  %3 = load %MkOrd, %MkOrd* %2, align 8
  %4 = extractvalue %MkOrd %3, 5
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @gt(%Ord* %x1){
entry:
  %0 = getelementptr %Ord, %Ord* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord* %x1 to %MkOrd*
  %3 = load %MkOrd, %MkOrd* %2, align 8
  %4 = extractvalue %MkOrd %3, 4
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @geq(%Ord* %x1){
entry:
  %0 = getelementptr %Ord, %Ord* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord* %x1 to %MkOrd*
  %3 = load %MkOrd, %MkOrd* %2, align 8
  %4 = extractvalue %MkOrd %3, 3
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @compare(%Ord* %x1){
entry:
  %0 = getelementptr %Ord, %Ord* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord* %x1 to %MkOrd*
  %3 = load %MkOrd, %MkOrd* %2, align 8
  %4 = extractvalue %MkOrd %3, 2
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @minus(%Group* %x1){
entry:
  %0 = getelementptr %Group, %Group* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Group* %x1 to %MkGroup*
  %3 = load %MkGroup, %MkGroup* %2, align 8
  %4 = extractvalue %MkGroup %3, 2
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc i8* @zero(%Monoid* %x1){
entry:
  %0 = getelementptr %Monoid, %Monoid* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Monoid* %x1 to %MkMonoid*
  %3 = load %MkMonoid, %MkMonoid* %2, align 8
  %4 = extractvalue %MkMonoid %3, 2
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi i8* [%4, %branch]
  ret i8* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %x1){
entry:
  %0 = getelementptr %Semigroup, %Semigroup* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Semigroup* %x1 to %MkSemigroup*
  %3 = load %MkSemigroup, %MkSemigroup* %2, align 8
  %4 = extractvalue %MkSemigroup %3, 1
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @fmap(%Functor* %x1){
entry:
  %0 = getelementptr %Functor, %Functor* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Functor* %x1 to %MkFunctor*
  %3 = load %MkFunctor, %MkFunctor* %2, align 8
  %4 = extractvalue %MkFunctor %3, 1
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @show(%Show* %x1){
entry:
  %0 = getelementptr %Show, %Show* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Show* %x1 to %MkShow*
  %3 = load %MkShow, %MkShow* %2, align 8
  %4 = extractvalue %MkShow %3, 1
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @eq(%Eq* %x1){
entry:
  %0 = getelementptr %Eq, %Eq* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Eq* %x1 to %MkEq*
  %3 = load %MkEq, %MkEq* %2, align 8
  %4 = extractvalue %MkEq %3, 1
  br label %switch_return
trivial_branch:
  %5 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %6 = phi {i8*, i8, [1 x i8*]}* [%4, %branch]
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc %Ordering* @callClosurelifted0({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = call ccc %Ordering* @lifted0(%Int* %5, %Int* %2)
  ret %Ordering* %6
}

define external ccc %Ordering* @lifted0(%Int* %x1, %Int* %x2){
entry:
  %0 = call ccc %Ord* @OrdInt()
  %1 = call ccc {i8*, i8, [1 x i8*]}* @lt(%Ord* %0)
  %2 = bitcast %Int* %x1 to i8*
  %3 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %1, i8* %2)
  %4 = bitcast i8* %3 to {i8*, i8, [1 x i8*]}*
  %5 = bitcast %Int* %x2 to i8*
  %6 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %4, i8* %5)
  %7 = bitcast i8* %6 to %Bool*
  %8 = getelementptr %Bool, %Bool* %7, i32 0, i32 0
  %9 = load i64, i64* %8, align 8
  switch i64 %9, label %default_branch1 [i64 1, label %branch i64 2, label %branch1]
branch:
  %10 = bitcast %Bool* %7 to %True*
  %11 = load %True, %True* %10, align 8
  %12 = call ccc %Ordering* @LE()
  br label %switch_return1
branch1:
  %13 = bitcast %Bool* %7 to %False*
  %14 = load %False, %False* %13, align 8
  %15 = call ccc %Ord* @OrdInt()
  %16 = call ccc {i8*, i8, [1 x i8*]}* @gt(%Ord* %15)
  %17 = bitcast %Int* %x1 to i8*
  %18 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %16, i8* %17)
  %19 = bitcast i8* %18 to {i8*, i8, [1 x i8*]}*
  %20 = bitcast %Int* %x2 to i8*
  %21 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %19, i8* %20)
  %22 = bitcast i8* %21 to %Bool*
  %23 = getelementptr %Bool, %Bool* %22, i32 0, i32 0
  %24 = load i64, i64* %23, align 8
  switch i64 %24, label %default_branch [i64 1, label %branch2 i64 2, label %branch3]
branch2:
  %25 = bitcast %Bool* %22 to %True*
  %26 = load %True, %True* %25, align 8
  %27 = call ccc %Ordering* @GE()
  br label %switch_return
branch3:
  %28 = bitcast %Bool* %22 to %False*
  %29 = load %False, %False* %28, align 8
  %30 = call ccc %Ordering* @EQ()
  br label %switch_return
default_branch:
  br label %switch_return
switch_return:
  %31 = phi %Ordering* [%27, %branch2], [%30, %branch3]
  br label %switch_return1
default_branch1:
  br label %switch_return1
switch_return1:
  %32 = phi %Ordering* [%12, %branch], [%31, %branch1]
  ret %Ordering* %32
}

define external ccc %Bool* @callClosurelifted1({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = call ccc %Bool* @lifted1(%Int* %5, %Int* %2)
  ret %Bool* %6
}

define external ccc %Bool* @lifted1(%Int* %x1, %Int* %x2){
entry:
  %0 = call ccc %Bool* @gtInt(%Int* %x1, %Int* %x2)
  ret %Bool* %0
}

define external ccc %Bool* @callClosurelifted2({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Bool**
  %2 = load %Bool*, %Bool** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Bool**
  %5 = load %Bool*, %Bool** %4, align 8
  %6 = call ccc %Bool* @lifted2(%Bool* %5, %Bool* %2)
  ret %Bool* %6
}

define external ccc %Bool* @lifted2(%Bool* %x1, %Bool* %x2){
entry:
  %0 = getelementptr %Bool, %Bool* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch2 [i64 1, label %branch i64 2, label %branch2]
branch:
  %2 = bitcast %Bool* %x1 to %False*
  %3 = load %False, %False* %2, align 8
  %4 = getelementptr %Bool, %Bool* %x2, i32 0, i32 0
  %5 = load i64, i64* %4, align 8
  switch i64 %5, label %trivial_branch [i64 1, label %branch1]
branch1:
  %6 = bitcast %Bool* %x2 to %False*
  %7 = load %False, %False* %6, align 8
  %8 = call ccc %Bool* @True()
  br label %switch_return
trivial_branch:
  %9 = call ccc %Bool* @False()
  br label %switch_return
switch_return:
  %10 = phi %Bool* [%8, %branch1]
  br label %switch_return2
branch2:
  %11 = bitcast %Bool* %x1 to %True*
  %12 = load %True, %True* %11, align 8
  %13 = getelementptr %Bool, %Bool* %x2, i32 0, i32 0
  %14 = load i64, i64* %13, align 8
  switch i64 %14, label %trivial_branch1 [i64 1, label %branch3]
branch3:
  %15 = bitcast %Bool* %x2 to %True*
  %16 = load %True, %True* %15, align 8
  %17 = call ccc %Bool* @True()
  br label %switch_return1
trivial_branch1:
  %18 = call ccc %Bool* @False()
  br label %switch_return1
switch_return1:
  %19 = phi %Bool* [%17, %branch3]
  br label %switch_return2
trivial_branch2:
  %20 = call ccc %Bool* @False()
  br label %switch_return2
switch_return2:
  %21 = phi %Bool* [%10, %branch], [%19, %branch2]
  ret %Bool* %21
}

define external ccc i8* @callClosurelifted3({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Maybe**
  %2 = load %Maybe*, %Maybe** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to i8**
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 3
  %7 = bitcast i8** %6 to {i8*, i8, [1 x i8*]}**
  %8 = load {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}** %7, align 8
  %9 = call ccc i8* @lifted3({i8*, i8, [1 x i8*]}* %8, i8* %5, %Maybe* %2)
  ret i8* %9
}

define external ccc i8* @lifted3({i8*, i8, [1 x i8*]}* %x1, i8* %x2, %Maybe* %x3){
entry:
  %0 = getelementptr %Maybe, %Maybe* %x3, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Maybe* %x3 to %Just*
  %3 = load %Just, %Just* %2, align 8
  %4 = extractvalue %Just %3, 1
  %5 = bitcast i8* %x2 to i8*
  %6 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %x1, i8* %5)
  %7 = bitcast i8* %6 to {i8*, i8, [1 x i8*]}*
  %8 = bitcast i8* %4 to i8*
  %9 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %7, i8* %8)
  %10 = bitcast i8* %9 to i8*
  br label %switch_return
branch1:
  %11 = bitcast %Maybe* %x3 to %Nothing*
  %12 = load %Nothing, %Nothing* %11, align 8
  br label %switch_return
trivial_branch:
  %13 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %14 = phi i8* [%10, %branch], [%x2, %branch1]
  ret i8* %14
}

define external ccc i8* @callClosurelifted4({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Maybe**
  %2 = load %Maybe*, %Maybe** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to i8**
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 3
  %7 = bitcast i8** %6 to {i8*, i8, [1 x i8*]}**
  %8 = load {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}** %7, align 8
  %9 = call ccc i8* @lifted4({i8*, i8, [1 x i8*]}* %8, i8* %5, %Maybe* %2)
  ret i8* %9
}

define external ccc i8* @lifted4({i8*, i8, [1 x i8*]}* %x1, i8* %x2, %Maybe* %x3){
entry:
  %0 = getelementptr %Maybe, %Maybe* %x3, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Maybe* %x3 to %Just*
  %3 = load %Just, %Just* %2, align 8
  %4 = extractvalue %Just %3, 1
  %5 = bitcast i8* %4 to i8*
  %6 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %x1, i8* %5)
  %7 = bitcast i8* %6 to {i8*, i8, [1 x i8*]}*
  %8 = bitcast i8* %x2 to i8*
  %9 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %7, i8* %8)
  %10 = bitcast i8* %9 to i8*
  br label %switch_return
branch1:
  %11 = bitcast %Maybe* %x3 to %Nothing*
  %12 = load %Nothing, %Nothing* %11, align 8
  br label %switch_return
trivial_branch:
  %13 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %14 = phi i8* [%10, %branch], [%x2, %branch1]
  ret i8* %14
}

define external ccc i8* @callClosurelifted5({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to i8**
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 3
  %7 = bitcast i8** %6 to {i8*, i8, [1 x i8*]}**
  %8 = load {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}** %7, align 8
  %9 = call ccc i8* @lifted5({i8*, i8, [1 x i8*]}* %8, i8* %5, %List* %2)
  ret i8* %9
}

define external ccc i8* @lifted5({i8*, i8, [1 x i8*]}* %x1, i8* %x2, %List* %x3){
entry:
  %0 = getelementptr %List, %List* %x3, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List* %x3 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = bitcast i8* %x2 to i8*
  %7 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %x1, i8* %6)
  %8 = bitcast i8* %7 to {i8*, i8, [1 x i8*]}*
  %9 = bitcast i8* %4 to i8*
  %10 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %8, i8* %9)
  %11 = bitcast i8* %10 to i8*
  %12 = call ccc %Foldable* @FoldableList()
  %13 = call ccc {i8*, i8, [1 x i8*]}* @foldl(%Foldable* %12)
  %14 = bitcast {i8*, i8, [1 x i8*]}* %x1 to i8*
  %15 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %13, i8* %14)
  %16 = bitcast i8* %15 to {i8*, i8, [1 x i8*]}*
  %17 = bitcast i8* %11 to i8*
  %18 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %16, i8* %17)
  %19 = bitcast i8* %18 to {i8*, i8, [1 x i8*]}*
  %20 = bitcast %List* %5 to i8*
  %21 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %19, i8* %20)
  %22 = bitcast i8* %21 to {i8*, i8, [1 x i8*]}*
  br label %switch_return
branch1:
  %23 = bitcast %List* %x3 to %Nil*
  %24 = load %Nil, %Nil* %23, align 8
  br label %switch_return
trivial_branch:
  %25 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %26 = phi {i8*, i8, [1 x i8*]}* [%22, %branch], [%x2, %branch1]
  ret {i8*, i8, [1 x i8*]}* %26
}

define external ccc i8* @callClosurelifted6({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to i8**
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 3
  %7 = bitcast i8** %6 to {i8*, i8, [1 x i8*]}**
  %8 = load {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}** %7, align 8
  %9 = call ccc i8* @lifted6({i8*, i8, [1 x i8*]}* %8, i8* %5, %List* %2)
  ret i8* %9
}

define external ccc i8* @lifted6({i8*, i8, [1 x i8*]}* %x1, i8* %x2, %List* %x3){
entry:
  %0 = getelementptr %List, %List* %x3, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List* %x3 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = call ccc %Foldable* @FoldableList()
  %7 = call ccc {i8*, i8, [1 x i8*]}* @foldr(%Foldable* %6)
  %8 = bitcast {i8*, i8, [1 x i8*]}* %x1 to i8*
  %9 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %7, i8* %8)
  %10 = bitcast i8* %9 to {i8*, i8, [1 x i8*]}*
  %11 = bitcast i8* %x2 to i8*
  %12 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %10, i8* %11)
  %13 = bitcast i8* %12 to {i8*, i8, [1 x i8*]}*
  %14 = bitcast %List* %5 to i8*
  %15 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %13, i8* %14)
  %16 = bitcast i8* %15 to {i8*, i8, [1 x i8*]}*
  %17 = bitcast i8* %4 to i8*
  %18 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %x1, i8* %17)
  %19 = bitcast i8* %18 to {i8*, i8, [1 x i8*]}*
  %20 = bitcast {i8*, i8, [1 x i8*]}* %16 to i8*
  %21 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %19, i8* %20)
  %22 = bitcast i8* %21 to i8*
  br label %switch_return
branch1:
  %23 = bitcast %List* %x3 to %Nil*
  %24 = load %Nil, %Nil* %23, align 8
  br label %switch_return
trivial_branch:
  %25 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %26 = phi i8* [%22, %branch], [%x2, %branch1]
  ret i8* %26
}

define external ccc %Either* @callClosurelifted7({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Either**
  %2 = load %Either*, %Either** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to {i8*, i8, [1 x i8*]}**
  %5 = load {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}** %4, align 8
  %6 = call ccc %Either* @lifted7({i8*, i8, [1 x i8*]}* %5, %Either* %2)
  ret %Either* %6
}

define external ccc %Either* @lifted7({i8*, i8, [1 x i8*]}* %x1, %Either* %x2){
entry:
  %0 = getelementptr %Either, %Either* %x2, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Either* %x2 to %Right*
  %3 = load %Right, %Right* %2, align 8
  %4 = extractvalue %Right %3, 1
  %5 = bitcast i8* %4 to i8*
  %6 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %x1, i8* %5)
  %7 = bitcast i8* %6 to i8*
  %8 = call ccc %Either* @Right(i8* %7)
  br label %switch_return1
trivial_branch:
  %9 = getelementptr %Either, %Either* %x2, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  switch i64 %10, label %trivial_branch1 [i64 1, label %branch1]
branch1:
  %11 = bitcast %Either* %x2 to %Left*
  %12 = load %Left, %Left* %11, align 8
  %13 = extractvalue %Left %12, 1
  %14 = call ccc %Either* @Left(i8* %13)
  br label %switch_return
trivial_branch1:
  %15 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %16 = phi %Either* [%14, %branch1]
  br label %switch_return1
switch_return1:
  %17 = phi %Either* [%8, %branch]
  ret %Either* %17
}

define external ccc %List* @callClosurelifted8({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to {i8*, i8, [1 x i8*]}**
  %5 = load {i8*, i8, [1 x i8*]}*, {i8*, i8, [1 x i8*]}** %4, align 8
  %6 = call ccc %List* @lifted8({i8*, i8, [1 x i8*]}* %5, %List* %2)
  ret %List* %6
}

define external ccc %List* @lifted8({i8*, i8, [1 x i8*]}* %x1, %List* %x2){
entry:
  %0 = getelementptr %List, %List* %x2, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List* %x2 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = bitcast i8* %4 to i8*
  %7 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %x1, i8* %6)
  %8 = bitcast i8* %7 to i8*
  %9 = call ccc %Functor* @FunctorList()
  %10 = call ccc {i8*, i8, [1 x i8*]}* @fmap(%Functor* %9)
  %11 = bitcast {i8*, i8, [1 x i8*]}* %x1 to i8*
  %12 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %10, i8* %11)
  %13 = bitcast i8* %12 to {i8*, i8, [1 x i8*]}*
  %14 = bitcast %List* %5 to i8*
  %15 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %13, i8* %14)
  %16 = bitcast i8* %15 to {i8*, i8, [1 x i8*]}*
  %17 = bitcast {i8*, i8, [1 x i8*]}* %16 to %List*
  %18 = call ccc %List* @Cons(i8* %8, %List* %17)
  br label %switch_return
branch1:
  %19 = bitcast %List* %x2 to %Nil*
  %20 = load %Nil, %Nil* %19, align 8
  %21 = call ccc %List* @Nil()
  br label %switch_return
trivial_branch:
  %22 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %23 = phi %List* [%18, %branch], [%21, %branch1]
  ret %List* %23
}

define external ccc %Int* @callClosurelifted9({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = call ccc %Int* @lifted9(%Int* %5, %Int* %2)
  ret %Int* %6
}

define external ccc %Int* @lifted9(%Int* %x1, %Int* %x2){
entry:
  %0 = call ccc %Int* @minusInt(%Int* %x1, %Int* %x2)
  ret %Int* %0
}

define external ccc %List* @callClosurelifted10({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %List**
  %5 = load %List*, %List** %4, align 8
  %6 = call ccc %List* @lifted10(%List* %5, %List* %2)
  ret %List* %6
}

define external ccc %List* @lifted10(%List* %x1, %List* %x2){
entry:
  %0 = getelementptr %List, %List* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch1 [i64 1, label %branch i64 2, label %branch2]
branch:
  %2 = bitcast %List* %x1 to %Cons*
  %3 = load %Cons, %Cons* %2, align 8
  %4 = extractvalue %Cons %3, 1
  %5 = extractvalue %Cons %3, 2
  %6 = getelementptr %List, %List* %x2, i32 0, i32 0
  %7 = load i64, i64* %6, align 8
  switch i64 %7, label %trivial_branch [i64 1, label %branch1]
branch1:
  %8 = bitcast %List* %x2 to %Nil*
  %9 = load %Nil, %Nil* %8, align 8
  %10 = call ccc %List* @Cons(i8* %4, %List* %5)
  br label %switch_return
trivial_branch:
  %11 = call ccc %Semigroup* @SemigroupList_a()
  %12 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %11)
  %13 = bitcast %List* %5 to i8*
  %14 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %12, i8* %13)
  %15 = bitcast i8* %14 to {i8*, i8, [1 x i8*]}*
  %16 = bitcast %List* %x2 to i8*
  %17 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %15, i8* %16)
  %18 = bitcast i8* %17 to i8*
  %19 = bitcast i8* %18 to %List*
  %20 = call ccc %List* @Cons(i8* %4, %List* %19)
  br label %switch_return
switch_return:
  %21 = phi %List* [%10, %branch1]
  br label %switch_return1
branch2:
  %22 = bitcast %List* %x1 to %Nil*
  %23 = load %Nil, %Nil* %22, align 8
  br label %switch_return1
trivial_branch1:
  %24 = call ccc i8* @failedPattern()
  br label %switch_return1
switch_return1:
  %25 = phi %List* [%21, %branch], [%x2, %branch2]
  ret %List* %25
}

define external ccc %Maybe* @callClosurelifted11({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Maybe**
  %2 = load %Maybe*, %Maybe** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Maybe**
  %5 = load %Maybe*, %Maybe** %4, align 8
  %6 = call ccc %Maybe* @lifted11(%Maybe* %5, %Maybe* %2)
  ret %Maybe* %6
}

define external ccc %Maybe* @lifted11(%Maybe* %x1, %Maybe* %x2){
entry:
  %0 = getelementptr %Maybe, %Maybe* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Maybe* %x1 to %Just*
  %3 = load %Just, %Just* %2, align 8
  %4 = extractvalue %Just %3, 1
  %5 = call ccc %Maybe* @Just(i8* %4)
  br label %switch_return
branch1:
  %6 = bitcast %Maybe* %x1 to %Nothing*
  %7 = load %Nothing, %Nothing* %6, align 8
  br label %switch_return
trivial_branch:
  %8 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %9 = phi %Maybe* [%5, %branch], [%x2, %branch1]
  ret %Maybe* %9
}

define external ccc %String* @callClosurelifted12({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %String**
  %2 = load %String*, %String** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %String**
  %5 = load %String*, %String** %4, align 8
  %6 = call ccc %String* @lifted12(%String* %5, %String* %2)
  ret %String* %6
}

define external ccc %String* @lifted12(%String* %x1, %String* %x2){
entry:
  %0 = call ccc %String* @plusStr(%String* %x1, %String* %x2)
  ret %String* %0
}

define external ccc %Int* @callClosurelifted13({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = call ccc %Int* @lifted13(%Int* %5, %Int* %2)
  ret %Int* %6
}

define external ccc %Int* @lifted13(%Int* %x1, %Int* %x2){
entry:
  %0 = call ccc %Int* @plusInt(%Int* %x1, %Int* %x2)
  ret %Int* %0
}

define external ccc %String* @callClosurelifted15({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Maybe**
  %2 = load %Maybe*, %Maybe** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Show**
  %5 = load %Show*, %Show** %4, align 8
  %6 = call ccc %String* @lifted15(%Show* %5, %Maybe* %2)
  ret %String* %6
}

define external ccc %String* @lifted15(%Show* %cvar14, %Maybe* %x1){
entry:
  %0 = getelementptr %Maybe, %Maybe* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Maybe* %x1 to %Just*
  %3 = load %Just, %Just* %2, align 8
  %4 = extractvalue %Just %3, 1
  %5 = call ccc {i8*, i8, [1 x i8*]}* @show(%Show* %cvar14)
  %6 = bitcast i8* %4 to i8*
  %7 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %5, i8* %6)
  %8 = bitcast i8* %7 to %String*
  %9 = call ccc %Semigroup* @SemigroupString()
  %10 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %9)
  %11 = call ccc %Int* @mkInt(i64 -97)
  %12 = bitcast %Int* %11 to i8*
  %13 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %10, i8* %12)
  %14 = bitcast i8* %13 to {i8*, i8, [1 x i8*]}*
  %15 = bitcast %String* %8 to i8*
  %16 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %14, i8* %15)
  %17 = bitcast i8* %16 to i8*
  br label %switch_return
branch1:
  %18 = bitcast %Maybe* %x1 to %Nothing*
  %19 = load %Nothing, %Nothing* %18, align 8
  %20 = call ccc %Int* @mkInt(i64 -97)
  br label %switch_return
trivial_branch:
  %21 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %22 = phi i8* [%17, %branch], [%20, %branch1]
  ret i8* %22
}

define external ccc %String* @lifted16(%Bool* %x1){
entry:
  %0 = getelementptr %Bool, %Bool* %x1, i32 0, i32 0
  %1 = load i64, i64* %0, align 8
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Bool* %x1 to %False*
  %3 = load %False, %False* %2, align 8
  %4 = call ccc %Int* @mkInt(i64 -97)
  br label %switch_return
branch1:
  %5 = bitcast %Bool* %x1 to %True*
  %6 = load %True, %True* %5, align 8
  %7 = call ccc %Int* @mkInt(i64 -97)
  br label %switch_return
trivial_branch:
  %8 = call ccc i8* @failedPattern()
  br label %switch_return
switch_return:
  %9 = phi %Int* [%4, %branch], [%7, %branch1]
  ret %Int* %9
}

define external ccc %String* @lifted17(%Int* %x1){
entry:
  %0 = call ccc %String* @showInt(%Int* %x1)
  ret %String* %0
}

define external ccc %String* @callClosurelifted19({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %List**
  %2 = load %List*, %List** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Show**
  %5 = load %Show*, %Show** %4, align 8
  %6 = call ccc %String* @lifted19(%Show* %5, %List* %2)
  ret %String* %6
}

define external ccc %String* @lifted19(%Show* %cvar18, %List* %x1){
entry:
  %0 = call ccc {i8*, i8, [1 x i8*]}* @show(%Show* %cvar18)
  %1 = call ccc %Functor* @FunctorList()
  %2 = call ccc {i8*, i8, [1 x i8*]}* @fmap(%Functor* %1)
  %3 = bitcast {i8*, i8, [1 x i8*]}* %0 to i8*
  %4 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %2, i8* %3)
  %5 = bitcast i8* %4 to {i8*, i8, [1 x i8*]}*
  %6 = bitcast %List* %x1 to i8*
  %7 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %5, i8* %6)
  %8 = bitcast i8* %7 to {i8*, i8, [1 x i8*]}*
  %9 = call ccc %Semigroup* @SemigroupString()
  %10 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %9)
  %11 = call ccc %Int* @mkInt(i64 -97)
  %12 = bitcast %Int* %11 to i8*
  %13 = bitcast {i8*, i8, [1 x i8*]}* %8 to %List*
  %14 = call ccc %List* @intersperse(i8* %12, %List* %13)
  %15 = call ccc %Foldable* @FoldableList()
  %16 = call ccc {i8*, i8, [1 x i8*]}* @foldr(%Foldable* %15)
  %17 = call ccc %Int* @mkInt(i64 -97)
  %18 = bitcast {i8*, i8, [1 x i8*]}* %10 to i8*
  %19 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %16, i8* %18)
  %20 = bitcast i8* %19 to {i8*, i8, [1 x i8*]}*
  %21 = bitcast %Int* %17 to i8*
  %22 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %20, i8* %21)
  %23 = bitcast i8* %22 to {i8*, i8, [1 x i8*]}*
  %24 = bitcast %List* %14 to i8*
  %25 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %23, i8* %24)
  %26 = bitcast i8* %25 to {i8*, i8, [1 x i8*]}*
  %27 = call ccc %Semigroup* @SemigroupString()
  %28 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %27)
  %29 = call ccc %Int* @mkInt(i64 -97)
  %30 = bitcast %Int* %29 to i8*
  %31 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %28, i8* %30)
  %32 = bitcast i8* %31 to {i8*, i8, [1 x i8*]}*
  %33 = bitcast {i8*, i8, [1 x i8*]}* %26 to i8*
  %34 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %32, i8* %33)
  %35 = bitcast i8* %34 to i8*
  %36 = call ccc %Semigroup* @SemigroupString()
  %37 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %36)
  %38 = call ccc %Int* @mkInt(i64 -97)
  %39 = bitcast i8* %35 to i8*
  %40 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %37, i8* %39)
  %41 = bitcast i8* %40 to {i8*, i8, [1 x i8*]}*
  %42 = bitcast %Int* %38 to i8*
  %43 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %41, i8* %42)
  %44 = bitcast i8* %43 to i8*
  ret i8* %44
}

define external ccc i8* @callClosurelifted21({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to i8**
  %2 = load i8*, i8** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to i8**
  %5 = load i8*, i8** %4, align 8
  %6 = call ccc i8* @lifted21(i8* %5, i8* %2)
  ret i8* %6
}

define external ccc i8* @lifted21(i8* %cvar20, i8* %x){
entry:
  ret i8* %cvar20
}

define external ccc %Int* @callClosurelifted22({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to i8**
  %2 = load i8*, i8** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = call ccc %Int* @lifted22(%Int* %5, i8* %2)
  ret %Int* %6
}

define external ccc %Int* @lifted22(%Int* %x, i8* %y){
entry:
  %0 = call ccc %Semigroup* @SemigroupInt()
  %1 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %0)
  %2 = call ccc %Int* @mkInt(i64 1)
  %3 = bitcast %Int* %x to i8*
  %4 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %1, i8* %3)
  %5 = bitcast i8* %4 to {i8*, i8, [1 x i8*]}*
  %6 = bitcast %Int* %2 to i8*
  %7 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %5, i8* %6)
  %8 = bitcast i8* %7 to i8*
  ret i8* %8
}

define external ccc %Int* @callClosurelifted24({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = call ccc %Int* @lifted24(%Int* %5, %Int* %2)
  ret %Int* %6
}

define external ccc %Int* @lifted24(%Int* %cvar23, %Int* %y){
entry:
  %0 = call ccc %Semigroup* @SemigroupInt()
  %1 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %0)
  %2 = bitcast %Int* %cvar23 to i8*
  %3 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %1, i8* %2)
  %4 = bitcast i8* %3 to {i8*, i8, [1 x i8*]}*
  %5 = bitcast %Int* %y to i8*
  %6 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %4, i8* %5)
  %7 = bitcast i8* %6 to i8*
  ret i8* %7
}

define external ccc %Int* @callClosurelifted28({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 3
  %7 = bitcast i8** %6 to %Int**
  %8 = load %Int*, %Int** %7, align 8
  %9 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 4
  %10 = bitcast i8** %9 to %Int**
  %11 = load %Int*, %Int** %10, align 8
  %12 = call ccc %Int* @lifted28(%Int* %11, %Int* %8, %Int* %5, %Int* %2)
  ret %Int* %12
}

define external ccc %Int* @lifted28(%Int* %cvar25, %Int* %cvar26, %Int* %cvar27, %Int* %b){
entry:
  %0 = call ccc %Semigroup* @SemigroupInt()
  %1 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %0)
  %2 = bitcast %Int* %cvar25 to i8*
  %3 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %1, i8* %2)
  %4 = bitcast i8* %3 to {i8*, i8, [1 x i8*]}*
  %5 = bitcast %Int* %b to i8*
  %6 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %4, i8* %5)
  %7 = bitcast i8* %6 to i8*
  %8 = call ccc %Semigroup* @SemigroupInt()
  %9 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %8)
  %10 = bitcast i8* %7 to i8*
  %11 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %9, i8* %10)
  %12 = bitcast i8* %11 to {i8*, i8, [1 x i8*]}*
  %13 = bitcast %Int* %cvar26 to i8*
  %14 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %12, i8* %13)
  %15 = bitcast i8* %14 to i8*
  %16 = call ccc %Semigroup* @SemigroupInt()
  %17 = call ccc {i8*, i8, [1 x i8*]}* @plus(%Semigroup* %16)
  %18 = bitcast i8* %15 to i8*
  %19 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %17, i8* %18)
  %20 = bitcast i8* %19 to {i8*, i8, [1 x i8*]}*
  %21 = bitcast %Int* %cvar27 to i8*
  %22 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %20, i8* %21)
  %23 = bitcast i8* %22 to i8*
  ret i8* %23
}

define external ccc {i8*, i8, [1 x i8*]}* @callClosurelifted30({i8*, i8, [1 x i8*]}* %closure){
entry:
  %0 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 1
  %1 = bitcast i8** %0 to %Int**
  %2 = load %Int*, %Int** %1, align 8
  %3 = getelementptr {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %closure, i32 0, i32 2, i32 2
  %4 = bitcast i8** %3 to %Int**
  %5 = load %Int*, %Int** %4, align 8
  %6 = call ccc {i8*, i8, [1 x i8*]}* @lifted30(%Int* %5, %Int* %2)
  ret {i8*, i8, [1 x i8*]}* %6
}

define external ccc {i8*, i8, [1 x i8*]}* @lifted30(%Int* %cvar29, %Int* %a){
entry:
  %0 = call ccc %Int* @mkInt(i64 2)
  %1 = call ccc i8* @malloc(i64 ptrtoint ({i8*, i8, [1 x i8*]}* getelementptr ({i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* zeroinitializer, i32 1) to i64))
  %2 = bitcast i8* %1 to {i8*, i8, [1 x i8*]}*
  %3 = load {i8*, i8, [1 x i8*]}, {i8*, i8, [1 x i8*]}* %2, align 8
  %4 = bitcast %Int* ({i8*, i8, [1 x i8*]}*)* @callClosurelifted28 to i8*
  %5 = insertvalue {i8*, i8, [1 x i8*]} %3, i8* %4, 0
  %6 = insertvalue {i8*, i8, [1 x i8*]} %5, i8 4, 1
  store {i8*, i8, [1 x i8*]} %6, {i8*, i8, [1 x i8*]}* %2, align 8
  %7 = bitcast %Int* %a to i8*
  %8 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %2, i8* %7)
  %9 = bitcast i8* %8 to {i8*, i8, [1 x i8*]}*
  %10 = bitcast %Int* %cvar29 to i8*
  %11 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %9, i8* %10)
  %12 = bitcast i8* %11 to {i8*, i8, [1 x i8*]}*
  %13 = bitcast %Int* %0 to i8*
  %14 = call ccc i8* @apply1({i8*, i8, [1 x i8*]}* %12, i8* %13)
  %15 = bitcast i8* %14 to {i8*, i8, [1 x i8*]}*
  ret {i8*, i8, [1 x i8*]}* %15
}

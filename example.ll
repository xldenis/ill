; ModuleID = 'example'

declare external ccc i8* @malloc(%size_t)

declare external ccc %Bool* @gtInt(%Int*, %Int*)

define external ccc %Double* @mkDouble(double %d){
entry:
  %0 = call ccc void* %malloc(i64 16)
  store double %d, void* %0, align 8
  ret void* %0
}

define external ccc %Int* @mkInt(i64 %d){
entry:
  %0 = call ccc void* %malloc(i64 16)
  store i64 %d, void* %0, align 8
  ret void* %0
}

%MkMonad = type {i64, %Applicative*, {void*, %m*, {void*, %a*}*}*}

define external ccc %MkMonad* @MkMonad(%Applicative* %a, {void*, %m*, {void*, %a*}*}* %a1){
entryC:
  %0 = call ccc void* %malloc(%MkMonad* getelementptr (%MkMonad, %MkMonad* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkMonad*
  %2 = load %MkMonad, %MkMonad* %1, align 8
  %3 = insertvalue %MkMonad %2, i64 0, 0
  %4 = insertvalue %MkMonad %2, %Applicative* %a, 1
  %5 = insertvalue %MkMonad %4, {void*, %m*, {void*, %a*}*}* %a1, 2
  store %MkMonad %5, %MkMonad* %1, align 8
  ret %MkMonad* %1
}

%MkApplicative = type {i64, %Functor*, {void*, %f*, %f*}*, {void*, %a*}*}

define external ccc %MkApplicative* @MkApplicative(%Functor* %a, {void*, %f*, %f*}* %a1, {void*, %a*}* %a2){
entryC:
  %0 = call ccc void* %malloc(%MkApplicative* getelementptr (%MkApplicative, %MkApplicative* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkApplicative*
  %2 = load %MkApplicative, %MkApplicative* %1, align 8
  %3 = insertvalue %MkApplicative %2, i64 0, 0
  %4 = insertvalue %MkApplicative %2, %Functor* %a, 1
  %5 = insertvalue %MkApplicative %4, {void*, %f*, %f*}* %a1, 2
  %6 = insertvalue %MkApplicative %5, {void*, %a*}* %a2, 3
  store %MkApplicative %6, %MkApplicative* %1, align 8
  ret %MkApplicative* %1
}

%MkBounded = type {i64, %a*, %a*}

define external ccc %MkBounded* @MkBounded(%a* %a, %a* %a1){
entryC:
  %0 = call ccc void* %malloc(%MkBounded* getelementptr (%MkBounded, %MkBounded* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkBounded*
  %2 = load %MkBounded, %MkBounded* %1, align 8
  %3 = insertvalue %MkBounded %2, i64 0, 0
  %4 = insertvalue %MkBounded %2, %a* %a, 1
  %5 = insertvalue %MkBounded %4, %a* %a1, 2
  store %MkBounded %5, %MkBounded* %1, align 8
  ret %MkBounded* %1
}

%MkEnum = type {i64, {void*, %a*}*, {void*, %a*}*, {void*, %a*}*, {void*, %Int*}*}

define external ccc %MkEnum* @MkEnum({void*, %a*}* %a, {void*, %a*}* %a1, {void*, %a*}* %a2, {void*, %Int*}* %a3){
entryC:
  %0 = call ccc void* %malloc(%MkEnum* getelementptr (%MkEnum, %MkEnum* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkEnum*
  %2 = load %MkEnum, %MkEnum* %1, align 8
  %3 = insertvalue %MkEnum %2, i64 0, 0
  %4 = insertvalue %MkEnum %2, {void*, %a*}* %a, 1
  %5 = insertvalue %MkEnum %4, {void*, %a*}* %a1, 2
  %6 = insertvalue %MkEnum %5, {void*, %a*}* %a2, 3
  %7 = insertvalue %MkEnum %6, {void*, %Int*}* %a3, 4
  store %MkEnum %7, %MkEnum* %1, align 8
  ret %MkEnum* %1
}

%MkFoldable = type {i64, {void*, {void*, %b*, %a*}*, %b*, %t*}*, {void*, {void*, %a*, %b*}*, %b*, %t*}*}

define external ccc %MkFoldable* @MkFoldable({void*, {void*, %b*, %a*}*, %b*, %t*}* %a, {void*, {void*, %a*, %b*}*, %b*, %t*}* %a1){
entryC:
  %0 = call ccc void* %malloc(%MkFoldable* getelementptr (%MkFoldable, %MkFoldable* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkFoldable*
  %2 = load %MkFoldable, %MkFoldable* %1, align 8
  %3 = insertvalue %MkFoldable %2, i64 0, 0
  %4 = insertvalue %MkFoldable %2, {void*, {void*, %b*, %a*}*, %b*, %t*}* %a, 1
  %5 = insertvalue %MkFoldable %4, {void*, {void*, %a*, %b*}*, %b*, %t*}* %a1, 2
  store %MkFoldable %5, %MkFoldable* %1, align 8
  ret %MkFoldable* %1
}

%MkOrd = type {i64, %Eq*, {void*, %a*, %a*}*, {void*, %a*, %a*}*, {void*, %a*, %a*}*, {void*, %a*, %a*}*, {void*, %a*, %a*}*, {void*, %a*, %a*}*, {void*, %a*, %a*}*}

define external ccc %MkOrd* @MkOrd(%Eq* %a, {void*, %a*, %a*}* %a1, {void*, %a*, %a*}* %a2, {void*, %a*, %a*}* %a3, {void*, %a*, %a*}* %a4, {void*, %a*, %a*}* %a5, {void*, %a*, %a*}* %a6, {void*, %a*, %a*}* %a7){
entryC:
  %0 = call ccc void* %malloc(%MkOrd* getelementptr (%MkOrd, %MkOrd* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkOrd*
  %2 = load %MkOrd, %MkOrd* %1, align 8
  %3 = insertvalue %MkOrd %2, i64 0, 0
  %4 = insertvalue %MkOrd %2, %Eq* %a, 1
  %5 = insertvalue %MkOrd %4, {void*, %a*, %a*}* %a1, 2
  %6 = insertvalue %MkOrd %5, {void*, %a*, %a*}* %a2, 3
  %7 = insertvalue %MkOrd %6, {void*, %a*, %a*}* %a3, 4
  %8 = insertvalue %MkOrd %7, {void*, %a*, %a*}* %a4, 5
  %9 = insertvalue %MkOrd %8, {void*, %a*, %a*}* %a5, 6
  %10 = insertvalue %MkOrd %9, {void*, %a*, %a*}* %a6, 7
  %11 = insertvalue %MkOrd %10, {void*, %a*, %a*}* %a7, 8
  store %MkOrd %11, %MkOrd* %1, align 8
  ret %MkOrd* %1
}

%MkGroup = type {i64, %Monoid*, {void*, %a*, %a*}*}

define external ccc %MkGroup* @MkGroup(%Monoid* %a, {void*, %a*, %a*}* %a1){
entryC:
  %0 = call ccc void* %malloc(%MkGroup* getelementptr (%MkGroup, %MkGroup* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkGroup*
  %2 = load %MkGroup, %MkGroup* %1, align 8
  %3 = insertvalue %MkGroup %2, i64 0, 0
  %4 = insertvalue %MkGroup %2, %Monoid* %a, 1
  %5 = insertvalue %MkGroup %4, {void*, %a*, %a*}* %a1, 2
  store %MkGroup %5, %MkGroup* %1, align 8
  ret %MkGroup* %1
}

%MkMonoid = type {i64, %Semigroup*, %a*}

define external ccc %MkMonoid* @MkMonoid(%Semigroup* %a, %a* %a1){
entryC:
  %0 = call ccc void* %malloc(%MkMonoid* getelementptr (%MkMonoid, %MkMonoid* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkMonoid*
  %2 = load %MkMonoid, %MkMonoid* %1, align 8
  %3 = insertvalue %MkMonoid %2, i64 0, 0
  %4 = insertvalue %MkMonoid %2, %Semigroup* %a, 1
  %5 = insertvalue %MkMonoid %4, %a* %a1, 2
  store %MkMonoid %5, %MkMonoid* %1, align 8
  ret %MkMonoid* %1
}

%MkSemigroup = type {i64, {void*, %a*, %a*}*}

define external ccc %MkSemigroup* @MkSemigroup({void*, %a*, %a*}* %a){
entryC:
  %0 = call ccc void* %malloc(%MkSemigroup* getelementptr (%MkSemigroup, %MkSemigroup* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkSemigroup*
  %2 = load %MkSemigroup, %MkSemigroup* %1, align 8
  %3 = insertvalue %MkSemigroup %2, i64 0, 0
  %4 = insertvalue %MkSemigroup %2, {void*, %a*, %a*}* %a, 1
  store %MkSemigroup %4, %MkSemigroup* %1, align 8
  ret %MkSemigroup* %1
}

%MkFunctor = type {i64, {void*, {void*, %a*}*, %f*}*}

define external ccc %MkFunctor* @MkFunctor({void*, {void*, %a*}*, %f*}* %a){
entryC:
  %0 = call ccc void* %malloc(%MkFunctor* getelementptr (%MkFunctor, %MkFunctor* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkFunctor*
  %2 = load %MkFunctor, %MkFunctor* %1, align 8
  %3 = insertvalue %MkFunctor %2, i64 0, 0
  %4 = insertvalue %MkFunctor %2, {void*, {void*, %a*}*, %f*}* %a, 1
  store %MkFunctor %4, %MkFunctor* %1, align 8
  ret %MkFunctor* %1
}

%MkShow = type {i64, {void*, %a*}*}

define external ccc %MkShow* @MkShow({void*, %a*}* %a){
entryC:
  %0 = call ccc void* %malloc(%MkShow* getelementptr (%MkShow, %MkShow* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkShow*
  %2 = load %MkShow, %MkShow* %1, align 8
  %3 = insertvalue %MkShow %2, i64 0, 0
  %4 = insertvalue %MkShow %2, {void*, %a*}* %a, 1
  store %MkShow %4, %MkShow* %1, align 8
  ret %MkShow* %1
}

%MkEq = type {i64, {void*, %a*, %a*}*}

define external ccc %MkEq* @MkEq({void*, %a*, %a*}* %a){
entryC:
  %0 = call ccc void* %malloc(%MkEq* getelementptr (%MkEq, %MkEq* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %MkEq*
  %2 = load %MkEq, %MkEq* %1, align 8
  %3 = insertvalue %MkEq %2, i64 0, 0
  %4 = insertvalue %MkEq %2, {void*, %a*, %a*}* %a, 1
  store %MkEq %4, %MkEq* %1, align 8
  ret %MkEq* %1
}

%True = type {i64}

define external ccc %True* @True(){
entryC:
  %0 = call ccc void* %malloc(%True* getelementptr (%True, %True* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %True*
  %2 = load %True, %True* %1, align 8
  %3 = insertvalue %True %2, i64 0, 0
  store %True %2, %True* %1, align 8
  ret %True* %1
}

%False = type {i64}

define external ccc %False* @False(){
entryC:
  %0 = call ccc void* %malloc(%False* getelementptr (%False, %False* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %False*
  %2 = load %False, %False* %1, align 8
  %3 = insertvalue %False %2, i64 0, 0
  store %False %2, %False* %1, align 8
  ret %False* %1
}

%Left = type {i64, %a*}

define external ccc %Left* @Left(%a* %a){
entryC:
  %0 = call ccc void* %malloc(%Left* getelementptr (%Left, %Left* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %Left*
  %2 = load %Left, %Left* %1, align 8
  %3 = insertvalue %Left %2, i64 0, 0
  %4 = insertvalue %Left %2, %a* %a, 1
  store %Left %4, %Left* %1, align 8
  ret %Left* %1
}

%Right = type {i64, %b*}

define external ccc %Right* @Right(%b* %a){
entryC:
  %0 = call ccc void* %malloc(%Right* getelementptr (%Right, %Right* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %Right*
  %2 = load %Right, %Right* %1, align 8
  %3 = insertvalue %Right %2, i64 0, 0
  %4 = insertvalue %Right %2, %b* %a, 1
  store %Right %4, %Right* %1, align 8
  ret %Right* %1
}

%Cons = type {i64, %a*, %List*}

define external ccc %Cons* @Cons(%a* %a, %List* %a1){
entryC:
  %0 = call ccc void* %malloc(%Cons* getelementptr (%Cons, %Cons* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %Cons*
  %2 = load %Cons, %Cons* %1, align 8
  %3 = insertvalue %Cons %2, i64 0, 0
  %4 = insertvalue %Cons %2, %a* %a, 1
  %5 = insertvalue %Cons %4, %List* %a1, 2
  store %Cons %5, %Cons* %1, align 8
  ret %Cons* %1
}

%Nil = type {i64}

define external ccc %Nil* @Nil(){
entryC:
  %0 = call ccc void* %malloc(%Nil* getelementptr (%Nil, %Nil* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %Nil*
  %2 = load %Nil, %Nil* %1, align 8
  %3 = insertvalue %Nil %2, i64 0, 0
  store %Nil %2, %Nil* %1, align 8
  ret %Nil* %1
}

%Just = type {i64, %a*}

define external ccc %Just* @Just(%a* %a){
entryC:
  %0 = call ccc void* %malloc(%Just* getelementptr (%Just, %Just* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %Just*
  %2 = load %Just, %Just* %1, align 8
  %3 = insertvalue %Just %2, i64 0, 0
  %4 = insertvalue %Just %2, %a* %a, 1
  store %Just %4, %Just* %1, align 8
  ret %Just* %1
}

%Nothing = type {i64}

define external ccc %Nothing* @Nothing(){
entryC:
  %0 = call ccc void* %malloc(%Nothing* getelementptr (%Nothing, %Nothing* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %Nothing*
  %2 = load %Nothing, %Nothing* %1, align 8
  %3 = insertvalue %Nothing %2, i64 0, 0
  store %Nothing %2, %Nothing* %1, align 8
  ret %Nothing* %1
}

%LE = type {i64}

define external ccc %LE* @LE(){
entryC:
  %0 = call ccc void* %malloc(%LE* getelementptr (%LE, %LE* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %LE*
  %2 = load %LE, %LE* %1, align 8
  %3 = insertvalue %LE %2, i64 0, 0
  store %LE %2, %LE* %1, align 8
  ret %LE* %1
}

%GE = type {i64}

define external ccc %GE* @GE(){
entryC:
  %0 = call ccc void* %malloc(%GE* getelementptr (%GE, %GE* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %GE*
  %2 = load %GE, %GE* %1, align 8
  %3 = insertvalue %GE %2, i64 0, 0
  store %GE %2, %GE* %1, align 8
  ret %GE* %1
}

%EQ = type {i64}

define external ccc %EQ* @EQ(){
entryC:
  %0 = call ccc void* %malloc(%EQ* getelementptr (%EQ, %EQ* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %EQ*
  %2 = load %EQ, %EQ* %1, align 8
  %3 = insertvalue %EQ %2, i64 0, 0
  store %EQ %2, %EQ* %1, align 8
  ret %EQ* %1
}

%T = type {i64, %a*, %b*}

define external ccc %T* @T(%a* %a, %b* %a1){
entryC:
  %0 = call ccc void* %malloc(%T* getelementptr (%T, %T* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %T*
  %2 = load %T, %T* %1, align 8
  %3 = insertvalue %T %2, i64 0, 0
  %4 = insertvalue %T %2, %a* %a, 1
  %5 = insertvalue %T %4, %b* %a1, 2
  store %T %5, %T* %1, align 8
  ret %T* %1
}

%Unit = type {i64}

define external ccc %Unit* @Unit(){
entryC:
  %0 = call ccc void* %malloc(%Unit* getelementptr (%Unit, %Unit* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to %Unit*
  %2 = load %Unit, %Unit* %1, align 8
  %3 = insertvalue %Unit %2, i64 0, 0
  store %Unit %2, %Unit* %1, align 8
  ret %Unit* %1
}

define external ccc %Functor* @FunctorEither_a(){
entry:
  %0 = call ccc void* %malloc({void*, {void*, %a*}*, %Either*}* getelementptr ({void*, {void*, %a*}*, %Either*}, {void*, {void*, %a*}*, %Either*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*, {void*, %a*}*, %Either*}*
  %2 = load {void*, {void*, %a*}*, %Either*}, {void*, {void*, %a*}*, %Either*}* %1, align 8
  %3 = insertvalue {void*, {void*, %a*}*, %Either*} %2, %Either* ({void*, {void*, %a*}*, %Either*}*) @callClosurelifted7, 0
  %4 = call ccc %Functor* @MkFunctor({void*, {void*, %a*}*, %Either*}* %1)
  ret %Functor* %4
}

define external ccc %Semigroup* @SemigroupList_a(){
entry:
  %0 = call ccc void* %malloc({void*, %List*, %List*}* getelementptr ({void*, %List*, %List*}, {void*, %List*, %List*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*, %List*, %List*}*
  %2 = load {void*, %List*, %List*}, {void*, %List*, %List*}* %1, align 8
  %3 = insertvalue {void*, %List*, %List*} %2, %List* ({void*, %List*, %List*}*) @callClosurelifted10, 0
  %4 = call ccc %Semigroup* @MkSemigroup({void*, %List*, %List*}* %1)
  ret %Semigroup* %4
}

define external ccc %Semigroup* @SemigroupMaybe_a(){
entry:
  %0 = call ccc void* %malloc({void*, %Maybe*, %Maybe*}* getelementptr ({void*, %Maybe*, %Maybe*}, {void*, %Maybe*, %Maybe*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*, %Maybe*, %Maybe*}*
  %2 = load {void*, %Maybe*, %Maybe*}, {void*, %Maybe*, %Maybe*}* %1, align 8
  %3 = insertvalue {void*, %Maybe*, %Maybe*} %2, %Maybe* ({void*, %Maybe*, %Maybe*}*) @callClosurelifted11, 0
  %4 = call ccc %Semigroup* @MkSemigroup({void*, %Maybe*, %Maybe*}* %1)
  ret %Semigroup* %4
}

define external ccc %Show* @callClosureShowMaybe_a({void*, %Show*}* %closure){
entry:
  %0 = getelementptr {void*, %Show*}, {void*, %Show*}* %closure, i32 0, i32 1
  %1 = load %Show*, %Show** %0, align 8
  %2 = call ccc %Show* @ShowMaybe_a(%Show* %1)
  ret %Show* %2
}

define external ccc %Show* @ShowMaybe_a(%Show* %x1){
entry:
  %0 = call ccc void* %malloc({void*, %Show*, %Maybe*}* getelementptr ({void*, %Show*, %Maybe*}, {void*, %Show*, %Maybe*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*, %Show*, %Maybe*}*
  %2 = load {void*, %Show*, %Maybe*}, {void*, %Show*, %Maybe*}* %1, align 8
  %3 = insertvalue {void*, %Show*, %Maybe*} %2, %String* ({void*, %Show*, %Maybe*}*) @callClosurelifted15, 0
  %4 = load {void*, %Show*, %Maybe*}, {void*, %Show*, %Maybe*}* %1, align 8
  %5 = insertvalue {void*, %Show*, %Maybe*} %4, %Show* %x1, 3
  store {void*, %Show*, %Maybe*} %5, {void*, %Show*, %Maybe*}* %1, align 8
  %6 = call ccc %Show* @MkShow({void*, %Show*, %Maybe*}* %1)
  ret %Show* %6
}

define external ccc %Show* @callClosureShowList_a({void*, %Show*}* %closure){
entry:
  %0 = getelementptr {void*, %Show*}, {void*, %Show*}* %closure, i32 0, i32 1
  %1 = load %Show*, %Show** %0, align 8
  %2 = call ccc %Show* @ShowList_a(%Show* %1)
  ret %Show* %2
}

define external ccc %Show* @ShowList_a(%Show* %x1){
entry:
  %0 = call ccc void* %malloc({void*, %Show*, %List*}* getelementptr ({void*, %Show*, %List*}, {void*, %Show*, %List*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*, %Show*, %List*}*
  %2 = load {void*, %Show*, %List*}, {void*, %Show*, %List*}* %1, align 8
  %3 = insertvalue {void*, %Show*, %List*} %2, %String* ({void*, %Show*, %List*}*) @callClosurelifted19, 0
  %4 = load {void*, %Show*, %List*}, {void*, %Show*, %List*}* %1, align 8
  %5 = insertvalue {void*, %Show*, %List*} %4, %Show* %x1, 3
  store {void*, %Show*, %List*} %5, {void*, %Show*, %List*}* %1, align 8
  %6 = call ccc %Show* @MkShow({void*, %Show*, %List*}* %1)
  ret %Show* %6
}

define external ccc %Bool* @callClosurenull({void*, %Foldable*, %t*}* %closure){
entry:
  %0 = getelementptr {void*, %Foldable*, %t*}, {void*, %Foldable*, %t*}* %closure, i32 0, i32 1
  %1 = load %Foldable*, %Foldable** %0, align 8
  %2 = getelementptr {void*, %Foldable*, %t*}, {void*, %Foldable*, %t*}* %closure, i32 0, i32 2
  %3 = load %t*, %t** %2, align 8
  %4 = call ccc %Bool* @null(%t* %3, %Foldable* %1)
  ret %Bool* %4
}

define external ccc %Bool* @null(%Foldable* %x1){
entry:
  %0 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*}*
  %2 = load {void*}, {void*}* %1, align 8
  %3 = insertvalue {void*} %2, %Bool* ({void*}*) @callClosureFalse, 0
  %4 = call ccc {void*, %b*}* @const({void*}* %1)
  %5 = call ccc {void*, %b*}* @const({void*, %b*}* %4)
  %6 = call ccc {void*, {void*, %a*, %b*}*, %b*, %t*}* @foldr(%Foldable* %x1)
  %7 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %8 = bitcast void* %7 to {void*}*
  %9 = load {void*}, {void*}* %8, align 8
  %10 = insertvalue {void*} %9, %Bool* ({void*}*) @callClosureTrue, 0
  %11 = load {void*, {void*, %a*, %b*}*, %b*, %t*}, {void*, {void*, %a*, %b*}*, %b*, %t*}* %6, align 8
  %12 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %11, %Foldable* %x1, 4
  %13 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %12, {void*, %b*}* %5, 3
  %14 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %13, {void*}* %8, 2
  store {void*, {void*, %a*, %b*}*, %b*, %t*} %14, {void*, {void*, %a*, %b*}*, %b*, %t*}* %6, align 8
  ret {void*, {void*, %a*, %b*}*, %b*, %t*}* %6
}

define external ccc %a* @callClosureconst({void*, %a*, %b*}* %closure){
entry:
  %0 = getelementptr {void*, %a*, %b*}, {void*, %a*, %b*}* %closure, i32 0, i32 1
  %1 = load %a*, %a** %0, align 8
  %2 = getelementptr {void*, %a*, %b*}, {void*, %a*, %b*}* %closure, i32 0, i32 2
  %3 = load %b*, %b** %2, align 8
  %4 = call ccc %a* @const(%b* %3, %a* %1)
  ret %a* %4
}

define external ccc %a* @const(%a* %x1){
entry:
  %0 = call ccc void* %malloc({void*, %a*, %b*}* getelementptr ({void*, %a*, %b*}, {void*, %a*, %b*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*, %a*, %b*}*
  %2 = load {void*, %a*, %b*}, {void*, %a*, %b*}* %1, align 8
  %3 = insertvalue {void*, %a*, %b*} %2, %a* ({void*, %a*, %b*}*) @callClosurelifted21, 0
  %4 = load {void*, %a*, %b*}, {void*, %a*, %b*}* %1, align 8
  %5 = insertvalue {void*, %a*, %b*} %4, %a* %x1, 3
  store {void*, %a*, %b*} %5, {void*, %a*, %b*}* %1, align 8
  ret {void*, %a*, %b*}* %1
}

define external ccc %Maybe* @callClosurehead({void*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, %List*}, {void*, %List*}* %closure, i32 0, i32 1
  %1 = load %List*, %List** %0, align 8
  %2 = call ccc %Maybe* @head(%List* %1)
  ret %Maybe* %2
}

define external ccc %Maybe* @head(%List* %x1){
entry:
  %0 = load %List, %List* %x1, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = call ccc %Maybe* @Just(%a* %3)
  br label %switch_return
branch1:
  %6 = bitcast %List %0 to %Nil
  %7 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %8 = bitcast void* %7 to {void*}*
  %9 = load {void*}, {void*}* %8, align 8
  %10 = insertvalue {void*} %9, %Maybe* ({void*}*) @callClosureNothing, 0
  br label %switch_return
trivial_branch:
  %11 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %12 = bitcast void* %11 to {void*}*
  %13 = load {void*}, {void*}* %12, align 8
  %14 = insertvalue {void*} %13, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %15 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %15
}

define external ccc %Maybe* @callClosureinit({void*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, %List*}, {void*, %List*}* %closure, i32 0, i32 1
  %1 = load %List*, %List** %0, align 8
  %2 = call ccc %Maybe* @init(%List* %1)
  ret %Maybe* %2
}

define external ccc %Maybe* @init(%List* %x1){
entry:
  %0 = load %List, %List* %x1, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch2 [i64 1, label %branch i64 2, label %branch4]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = load %List, %List* %4, align 8
  %6 = extractvalue %List %5, 0
  switch i64 %6, label %trivial_branch [i64 1, label %branch1]
branch1:
  %7 = bitcast %List %5 to %Nil
  %8 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %9 = bitcast void* %8 to {void*}*
  %10 = load {void*}, {void*}* %9, align 8
  %11 = insertvalue {void*} %10, %List* ({void*}*) @callClosureNil, 0
  %12 = call ccc %Maybe* @Just({void*}* %9)
  br label %switch_return1
trivial_branch:
  %13 = call ccc %Maybe* @init(%List* %4)
  %14 = load %Maybe, %Maybe* %13, align 8
  %15 = extractvalue %Maybe %14, 0
  switch i64 %15, label %trivial_branch1 [i64 1, label %branch2 i64 2, label %branch3]
branch2:
  %16 = bitcast %Maybe %14 to %Just
  %17 = extractvalue %Just %16, 1
  %18 = call ccc %List* @Cons(%a* %3, %List* %17)
  %19 = call ccc %Maybe* @Just(%List* %18)
  br label %switch_return
branch3:
  %20 = bitcast %Maybe %14 to %Nothing
  %21 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %22 = bitcast void* %21 to {void*}*
  %23 = load {void*}, {void*}* %22, align 8
  %24 = insertvalue {void*} %23, %List* ({void*}*) @callClosureNil, 0
  %25 = call ccc %List* @Cons(%a* %3, {void*}* %22)
  %26 = call ccc %Maybe* @Just(%List* %25)
  br label %switch_return
trivial_branch1:
  %27 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %28 = bitcast void* %27 to {void*}*
  %29 = load {void*}, {void*}* %28, align 8
  %30 = insertvalue {void*} %29, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %31 = phi i64 [5, %branch2], [5, %branch3]
  br label %switch_return1
switch_return1:
  %32 = phi i64 [5, %branch1]
  br label %switch_return2
branch4:
  %33 = bitcast %List %0 to %Nil
  %34 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %35 = bitcast void* %34 to {void*}*
  %36 = load {void*}, {void*}* %35, align 8
  %37 = insertvalue {void*} %36, %Maybe* ({void*}*) @callClosureNothing, 0
  br label %switch_return2
trivial_branch2:
  %38 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %39 = bitcast void* %38 to {void*}*
  %40 = load {void*}, {void*}* %39, align 8
  %41 = insertvalue {void*} %40, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return2
switch_return2:
  %42 = phi i64 [5, %branch], [5, %branch4]
  ret i64 %42
}

define external ccc %List* @callClosureintersperse({void*, %a*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, %a*, %List*}, {void*, %a*, %List*}* %closure, i32 0, i32 1
  %1 = load %a*, %a** %0, align 8
  %2 = getelementptr {void*, %a*, %List*}, {void*, %a*, %List*}* %closure, i32 0, i32 2
  %3 = load %List*, %List** %2, align 8
  %4 = call ccc %List* @intersperse(%List* %3, %a* %1)
  ret %List* %4
}

define external ccc %List* @intersperse(%a* %x1, %List* %x2){
entry:
  %0 = load %List, %List* %x2, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch1 [i64 1, label %branch i64 2, label %branch2]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = load %List, %List* %4, align 8
  %6 = extractvalue %List %5, 0
  switch i64 %6, label %trivial_branch [i64 1, label %branch1]
branch1:
  %7 = bitcast %List %5 to %Nil
  %8 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %9 = bitcast void* %8 to {void*}*
  %10 = load {void*}, {void*}* %9, align 8
  %11 = insertvalue {void*} %10, %List* ({void*}*) @callClosureNil, 0
  %12 = call ccc %List* @Cons(%a* %3, {void*}* %9)
  br label %switch_return
trivial_branch:
  %13 = call ccc %List* @intersperse(%a* %x1, %List* %4)
  %14 = call ccc %List* @Cons(%a* %x1, %List* %13)
  %15 = call ccc %List* @Cons(%a* %3, %List* %14)
  br label %switch_return
switch_return:
  %16 = phi i64 [5, %branch1]
  br label %switch_return1
branch2:
  %17 = bitcast %List %0 to %Nil
  %18 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %19 = bitcast void* %18 to {void*}*
  %20 = load {void*}, {void*}* %19, align 8
  %21 = insertvalue {void*} %20, %List* ({void*}*) @callClosureNil, 0
  br label %switch_return1
trivial_branch1:
  %22 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %23 = bitcast void* %22 to {void*}*
  %24 = load {void*}, {void*}* %23, align 8
  %25 = insertvalue {void*} %24, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return1
switch_return1:
  %26 = phi i64 [5, %branch], [5, %branch2]
  ret i64 %26
}

define external ccc %Maybe* @callClosurelast({void*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, %List*}, {void*, %List*}* %closure, i32 0, i32 1
  %1 = load %List*, %List** %0, align 8
  %2 = call ccc %Maybe* @last(%List* %1)
  ret %Maybe* %2
}

define external ccc %Maybe* @last(%List* %x1){
entry:
  %0 = load %List, %List* %x1, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = call ccc %Maybe* @Just(%a* %3)
  br label %switch_return
branch1:
  %6 = bitcast %List %0 to %Nil
  %7 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %8 = bitcast void* %7 to {void*}*
  %9 = load {void*}, {void*}* %8, align 8
  %10 = insertvalue {void*} %9, %Maybe* ({void*}*) @callClosureNothing, 0
  br label %switch_return
trivial_branch:
  %11 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %12 = bitcast void* %11 to {void*}*
  %13 = load {void*}, {void*}* %12, align 8
  %14 = insertvalue {void*} %13, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %15 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %15
}

define external ccc %Int* @callClosurelength({void*, %Foldable*, %t*}* %closure){
entry:
  %0 = getelementptr {void*, %Foldable*, %t*}, {void*, %Foldable*, %t*}* %closure, i32 0, i32 1
  %1 = load %Foldable*, %Foldable** %0, align 8
  %2 = getelementptr {void*, %Foldable*, %t*}, {void*, %Foldable*, %t*}* %closure, i32 0, i32 2
  %3 = load %t*, %t** %2, align 8
  %4 = call ccc %Int* @length(%t* %3, %Foldable* %1)
  ret %Int* %4
}

define external ccc %Int* @length(%Foldable* %x1, %t* %x2){
entry:
  %0 = call ccc {void*, {void*, %b*, %a*}*, %b*, %t*}* @foldl(%Foldable* %x1)
  %1 = call ccc void* %malloc({void*, %Int*, %a*}* getelementptr ({void*, %Int*, %a*}, {void*, %Int*, %a*}* zeroinitializer, i32 1))
  %2 = bitcast void* %1 to {void*, %Int*, %a*}*
  %3 = load {void*, %Int*, %a*}, {void*, %Int*, %a*}* %2, align 8
  %4 = insertvalue {void*, %Int*, %a*} %3, %Int* ({void*, %Int*, %a*}*) @callClosurelifted22, 0
  %5 = call ccc %Int* @mkInt(i64 0)
  %6 = load {void*, {void*, %b*, %a*}*, %b*, %t*}, {void*, {void*, %b*, %a*}*, %b*, %t*}* %0, align 8
  %7 = insertvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %6, %Foldable* %x1, 4
  %8 = insertvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %7, {void*, %Int*, %a*}* %2, 3
  %9 = insertvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %8, %Int* %5, 2
  %10 = insertvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %9, %t* %x2, 1
  store {void*, {void*, %b*, %a*}*, %b*, %t*} %10, {void*, {void*, %b*, %a*}*, %b*, %t*}* %0, align 8
  %11 = extractvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %6, 0
  %12 = bitcast void* %11 to void* ({void*, {void*, %b*, %a*}*, %b*, %t*}*)*
  %13 = call ccc void* %12({void*, {void*, %b*, %a*}*, %b*, %t*}* %0)
  ret void* %13
}

define external ccc %Maybe* @callClosuretail({void*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, %List*}, {void*, %List*}* %closure, i32 0, i32 1
  %1 = load %List*, %List** %0, align 8
  %2 = call ccc %Maybe* @tail(%List* %1)
  ret %Maybe* %2
}

define external ccc %Maybe* @tail(%List* %x1){
entry:
  %0 = load %List, %List* %x1, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = call ccc %Maybe* @Just(%List* %4)
  br label %switch_return
branch1:
  %6 = bitcast %List %0 to %Nil
  %7 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %8 = bitcast void* %7 to {void*}*
  %9 = load {void*}, {void*}* %8, align 8
  %10 = insertvalue {void*} %9, %Maybe* ({void*}*) @callClosureNothing, 0
  br label %switch_return
trivial_branch:
  %11 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %12 = bitcast void* %11 to {void*}*
  %13 = load {void*}, {void*}* %12, align 8
  %14 = insertvalue {void*} %13, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %15 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %15
}

define external ccc %Maybe* @callClosureuncons({void*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, %List*}, {void*, %List*}* %closure, i32 0, i32 1
  %1 = load %List*, %List** %0, align 8
  %2 = call ccc %Maybe* @uncons(%List* %1)
  ret %Maybe* %2
}

define external ccc %Maybe* @uncons(%List* %x1){
entry:
  %0 = load %List, %List* %x1, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = call ccc %Tuple* @T(%a* %3, %List* %4)
  %6 = call ccc %Maybe* @Just(%Tuple* %5)
  br label %switch_return
branch1:
  %7 = bitcast %List %0 to %Nil
  %8 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %9 = bitcast void* %8 to {void*}*
  %10 = load {void*}, {void*}* %9, align 8
  %11 = insertvalue {void*} %10, %Maybe* ({void*}*) @callClosureNothing, 0
  br label %switch_return
trivial_branch:
  %12 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %13 = bitcast void* %12 to {void*}*
  %14 = load {void*}, {void*}* %13, align 8
  %15 = insertvalue {void*} %14, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %16 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %16
}

define external ccc %List* @callClosurezipWith({void*, {void*, %a*, %b*}*, %List*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, {void*, %a*, %b*}*, %List*, %List*}, {void*, {void*, %a*, %b*}*, %List*, %List*}* %closure, i32 0, i32 1
  %1 = load {void*, %a*, %b*}*, {void*, %a*, %b*}** %0, align 8
  %2 = getelementptr {void*, {void*, %a*, %b*}*, %List*, %List*}, {void*, {void*, %a*, %b*}*, %List*, %List*}* %closure, i32 0, i32 2
  %3 = load %List*, %List** %2, align 8
  %4 = getelementptr {void*, {void*, %a*, %b*}*, %List*, %List*}, {void*, {void*, %a*, %b*}*, %List*, %List*}* %closure, i32 0, i32 3
  %5 = load %List*, %List** %4, align 8
  %6 = call ccc %List* @zipWith(%List* %5, %List* %3, {void*, %a*, %b*}* %1)
  ret %List* %6
}

define external ccc %List* @zipWith({void*, %a*, %b*}* %x1, %List* %x2, %List* %x3){
entry:
  %0 = load %List, %List* %x2, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch1 [i64 1, label %branch]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = load %List, %List* %x3, align 8
  %6 = extractvalue %List %5, 0
  switch i64 %6, label %trivial_branch [i64 1, label %branch1]
branch1:
  %7 = bitcast %List %5 to %Cons
  %8 = extractvalue %Cons %7, 1
  %9 = extractvalue %Cons %7, 2
  %10 = load {void*, %a*, %b*}, {void*, %a*, %b*}* %x1, align 8
  %11 = insertvalue {void*, %a*, %b*} %10, %a* %3, 3
  %12 = insertvalue {void*, %a*, %b*} %11, %b* %8, 2
  store {void*, %a*, %b*} %12, {void*, %a*, %b*}* %x1, align 8
  %13 = call ccc %List* @zipWith({void*, %a*, %b*}* %x1, %List* %4, %List* %9)
  %14 = call ccc %List* @Cons({void*, %a*, %b*}* %x1, %List* %13)
  br label %switch_return
trivial_branch:
  %15 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %16 = bitcast void* %15 to {void*}*
  %17 = load {void*}, {void*}* %16, align 8
  %18 = insertvalue {void*} %17, %List* ({void*}*) @callClosureNil, 0
  br label %switch_return
switch_return:
  %19 = phi i64 [5, %branch1]
  br label %switch_return1
trivial_branch1:
  %20 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %21 = bitcast void* %20 to {void*}*
  %22 = load {void*}, {void*}* %21, align 8
  %23 = insertvalue {void*} %22, %List* ({void*}*) @callClosureNil, 0
  br label %switch_return1
switch_return1:
  %24 = phi i64 [5, %branch]
  ret i64 %24
}

define external ccc {void*, %m*, {void*, %a*}*}* @callClosurebind({void*, %Monad*}* %closure){
entry:
  %0 = getelementptr {void*, %Monad*}, {void*, %Monad*}* %closure, i32 0, i32 1
  %1 = load %Monad*, %Monad** %0, align 8
  %2 = call ccc {void*, %m*, {void*, %a*}*}* @bind(%Monad* %1)
  ret {void*, %m*, {void*, %a*}*}* %2
}

define external ccc {void*, %m*, {void*, %a*}*}* @bind(%Monad* %x1){
entry:
  %0 = load %Monad, %Monad* %x1, align 8
  %1 = extractvalue %Monad %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Monad %0 to %MkMonad
  %3 = extractvalue %MkMonad %2, 2
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc {void*, %a*}* @callClosurepure({void*, %Applicative*}* %closure){
entry:
  %0 = getelementptr {void*, %Applicative*}, {void*, %Applicative*}* %closure, i32 0, i32 1
  %1 = load %Applicative*, %Applicative** %0, align 8
  %2 = call ccc {void*, %a*}* @pure(%Applicative* %1)
  ret {void*, %a*}* %2
}

define external ccc {void*, %a*}* @pure(%Applicative* %x1){
entry:
  %0 = load %Applicative, %Applicative* %x1, align 8
  %1 = extractvalue %Applicative %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Applicative %0 to %MkApplicative
  %3 = extractvalue %MkApplicative %2, 3
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc {void*, %f*, %f*}* @callClosureap({void*, %Applicative*}* %closure){
entry:
  %0 = getelementptr {void*, %Applicative*}, {void*, %Applicative*}* %closure, i32 0, i32 1
  %1 = load %Applicative*, %Applicative** %0, align 8
  %2 = call ccc {void*, %f*, %f*}* @ap(%Applicative* %1)
  ret {void*, %f*, %f*}* %2
}

define external ccc {void*, %f*, %f*}* @ap(%Applicative* %x1){
entry:
  %0 = load %Applicative, %Applicative* %x1, align 8
  %1 = extractvalue %Applicative %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Applicative %0 to %MkApplicative
  %3 = extractvalue %MkApplicative %2, 2
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosureminBound({void*, %Bounded*}* %closure){
entry:
  %0 = getelementptr {void*, %Bounded*}, {void*, %Bounded*}* %closure, i32 0, i32 1
  %1 = load %Bounded*, %Bounded** %0, align 8
  %2 = call ccc %a* @minBound(%Bounded* %1)
  ret %a* %2
}

define external ccc %a* @minBound(%Bounded* %x1){
entry:
  %0 = load %Bounded, %Bounded* %x1, align 8
  %1 = extractvalue %Bounded %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Bounded %0 to %MkBounded
  %3 = extractvalue %MkBounded %2, 2
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosuremaxBound({void*, %Bounded*}* %closure){
entry:
  %0 = getelementptr {void*, %Bounded*}, {void*, %Bounded*}* %closure, i32 0, i32 1
  %1 = load %Bounded*, %Bounded** %0, align 8
  %2 = call ccc %a* @maxBound(%Bounded* %1)
  ret %a* %2
}

define external ccc %a* @maxBound(%Bounded* %x1){
entry:
  %0 = load %Bounded, %Bounded* %x1, align 8
  %1 = extractvalue %Bounded %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Bounded %0 to %MkBounded
  %3 = extractvalue %MkBounded %2, 1
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosuretoEnum({void*, %Enum*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Enum*, %Int*}, {void*, %Enum*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Enum*, %Enum** %0, align 8
  %2 = getelementptr {void*, %Enum*, %Int*}, {void*, %Enum*, %Int*}* %closure, i32 0, i32 2
  %3 = load %Int*, %Int** %2, align 8
  %4 = call ccc %a* @toEnum(%Int* %3, %Enum* %1)
  ret %a* %4
}

define external ccc %a* @toEnum(%Enum* %x1){
entry:
  %0 = load %Enum, %Enum* %x1, align 8
  %1 = extractvalue %Enum %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Enum %0 to %MkEnum
  %3 = extractvalue %MkEnum %2, 4
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosuresucc({void*, %Enum*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Enum*, %a*}, {void*, %Enum*, %a*}* %closure, i32 0, i32 1
  %1 = load %Enum*, %Enum** %0, align 8
  %2 = getelementptr {void*, %Enum*, %a*}, {void*, %Enum*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = call ccc %a* @succ(%a* %3, %Enum* %1)
  ret %a* %4
}

define external ccc %a* @succ(%Enum* %x1){
entry:
  %0 = load %Enum, %Enum* %x1, align 8
  %1 = extractvalue %Enum %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Enum %0 to %MkEnum
  %3 = extractvalue %MkEnum %2, 3
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosurepred({void*, %Enum*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Enum*, %a*}, {void*, %Enum*, %a*}* %closure, i32 0, i32 1
  %1 = load %Enum*, %Enum** %0, align 8
  %2 = getelementptr {void*, %Enum*, %a*}, {void*, %Enum*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = call ccc %a* @pred(%a* %3, %Enum* %1)
  ret %a* %4
}

define external ccc %a* @pred(%Enum* %x1){
entry:
  %0 = load %Enum, %Enum* %x1, align 8
  %1 = extractvalue %Enum %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Enum %0 to %MkEnum
  %3 = extractvalue %MkEnum %2, 2
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %Int* @callClosurefromEnum({void*, %Enum*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Enum*, %a*}, {void*, %Enum*, %a*}* %closure, i32 0, i32 1
  %1 = load %Enum*, %Enum** %0, align 8
  %2 = getelementptr {void*, %Enum*, %a*}, {void*, %Enum*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = call ccc %Int* @fromEnum(%a* %3, %Enum* %1)
  ret %Int* %4
}

define external ccc %Int* @fromEnum(%Enum* %x1){
entry:
  %0 = load %Enum, %Enum* %x1, align 8
  %1 = extractvalue %Enum %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Enum %0 to %MkEnum
  %3 = extractvalue %MkEnum %2, 1
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc {void*, {void*, %a*, %b*}*, %b*, %t*}* @callClosurefoldr({void*, %Foldable*}* %closure){
entry:
  %0 = getelementptr {void*, %Foldable*}, {void*, %Foldable*}* %closure, i32 0, i32 1
  %1 = load %Foldable*, %Foldable** %0, align 8
  %2 = call ccc {void*, {void*, %a*, %b*}*, %b*, %t*}* @foldr(%Foldable* %1)
  ret {void*, {void*, %a*, %b*}*, %b*, %t*}* %2
}

define external ccc {void*, {void*, %a*, %b*}*, %b*, %t*}* @foldr(%Foldable* %x1){
entry:
  %0 = load %Foldable, %Foldable* %x1, align 8
  %1 = extractvalue %Foldable %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Foldable %0 to %MkFoldable
  %3 = extractvalue %MkFoldable %2, 2
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc {void*, {void*, %b*, %a*}*, %b*, %t*}* @callClosurefoldl({void*, %Foldable*}* %closure){
entry:
  %0 = getelementptr {void*, %Foldable*}, {void*, %Foldable*}* %closure, i32 0, i32 1
  %1 = load %Foldable*, %Foldable** %0, align 8
  %2 = call ccc {void*, {void*, %b*, %a*}*, %b*, %t*}* @foldl(%Foldable* %1)
  ret {void*, {void*, %b*, %a*}*, %b*, %t*}* %2
}

define external ccc {void*, {void*, %b*, %a*}*, %b*, %t*}* @foldl(%Foldable* %x1){
entry:
  %0 = load %Foldable, %Foldable* %x1, align 8
  %1 = extractvalue %Foldable %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Foldable %0 to %MkFoldable
  %3 = extractvalue %MkFoldable %2, 1
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosuremin({void*, %Ord*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Ord*, %Ord** %0, align 8
  %2 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %a* @min(%a* %5, %a* %3, %Ord* %1)
  ret %a* %6
}

define external ccc %a* @min(%Ord* %x1){
entry:
  %0 = load %Ord, %Ord* %x1, align 8
  %1 = extractvalue %Ord %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord %0 to %MkOrd
  %3 = extractvalue %MkOrd %2, 8
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosuremax({void*, %Ord*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Ord*, %Ord** %0, align 8
  %2 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %a* @max(%a* %5, %a* %3, %Ord* %1)
  ret %a* %6
}

define external ccc %a* @max(%Ord* %x1){
entry:
  %0 = load %Ord, %Ord* %x1, align 8
  %1 = extractvalue %Ord %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord %0 to %MkOrd
  %3 = extractvalue %MkOrd %2, 7
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %Bool* @callClosurelt({void*, %Ord*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Ord*, %Ord** %0, align 8
  %2 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %Bool* @lt(%a* %5, %a* %3, %Ord* %1)
  ret %Bool* %6
}

define external ccc %Bool* @lt(%Ord* %x1){
entry:
  %0 = load %Ord, %Ord* %x1, align 8
  %1 = extractvalue %Ord %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord %0 to %MkOrd
  %3 = extractvalue %MkOrd %2, 6
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %Bool* @callClosureleq({void*, %Ord*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Ord*, %Ord** %0, align 8
  %2 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %Bool* @leq(%a* %5, %a* %3, %Ord* %1)
  ret %Bool* %6
}

define external ccc %Bool* @leq(%Ord* %x1){
entry:
  %0 = load %Ord, %Ord* %x1, align 8
  %1 = extractvalue %Ord %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord %0 to %MkOrd
  %3 = extractvalue %MkOrd %2, 5
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %Bool* @callClosuregt({void*, %Ord*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Ord*, %Ord** %0, align 8
  %2 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %Bool* @gt(%a* %5, %a* %3, %Ord* %1)
  ret %Bool* %6
}

define external ccc %Bool* @gt(%Ord* %x1){
entry:
  %0 = load %Ord, %Ord* %x1, align 8
  %1 = extractvalue %Ord %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord %0 to %MkOrd
  %3 = extractvalue %MkOrd %2, 4
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %Bool* @callClosuregeq({void*, %Ord*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Ord*, %Ord** %0, align 8
  %2 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %Bool* @geq(%a* %5, %a* %3, %Ord* %1)
  ret %Bool* %6
}

define external ccc %Bool* @geq(%Ord* %x1){
entry:
  %0 = load %Ord, %Ord* %x1, align 8
  %1 = extractvalue %Ord %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord %0 to %MkOrd
  %3 = extractvalue %MkOrd %2, 3
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %Ordering* @callClosurecompare({void*, %Ord*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Ord*, %Ord** %0, align 8
  %2 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Ord*, %a*, %a*}, {void*, %Ord*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %Ordering* @compare(%a* %5, %a* %3, %Ord* %1)
  ret %Ordering* %6
}

define external ccc %Ordering* @compare(%Ord* %x1){
entry:
  %0 = load %Ord, %Ord* %x1, align 8
  %1 = extractvalue %Ord %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Ord %0 to %MkOrd
  %3 = extractvalue %MkOrd %2, 2
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosureminus({void*, %Group*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Group*, %a*, %a*}, {void*, %Group*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Group*, %Group** %0, align 8
  %2 = getelementptr {void*, %Group*, %a*, %a*}, {void*, %Group*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Group*, %a*, %a*}, {void*, %Group*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %a* @minus(%a* %5, %a* %3, %Group* %1)
  ret %a* %6
}

define external ccc %a* @minus(%Group* %x1){
entry:
  %0 = load %Group, %Group* %x1, align 8
  %1 = extractvalue %Group %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Group %0 to %MkGroup
  %3 = extractvalue %MkGroup %2, 2
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosurezero({void*, %Monoid*}* %closure){
entry:
  %0 = getelementptr {void*, %Monoid*}, {void*, %Monoid*}* %closure, i32 0, i32 1
  %1 = load %Monoid*, %Monoid** %0, align 8
  %2 = call ccc %a* @zero(%Monoid* %1)
  ret %a* %2
}

define external ccc %a* @zero(%Monoid* %x1){
entry:
  %0 = load %Monoid, %Monoid* %x1, align 8
  %1 = extractvalue %Monoid %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Monoid %0 to %MkMonoid
  %3 = extractvalue %MkMonoid %2, 2
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %a* @callClosureplus({void*, %Semigroup*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Semigroup*, %a*, %a*}, {void*, %Semigroup*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Semigroup*, %Semigroup** %0, align 8
  %2 = getelementptr {void*, %Semigroup*, %a*, %a*}, {void*, %Semigroup*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Semigroup*, %a*, %a*}, {void*, %Semigroup*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %a* @plus(%a* %5, %a* %3, %Semigroup* %1)
  ret %a* %6
}

define external ccc %a* @plus(%Semigroup* %x1){
entry:
  %0 = load %Semigroup, %Semigroup* %x1, align 8
  %1 = extractvalue %Semigroup %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Semigroup %0 to %MkSemigroup
  %3 = extractvalue %MkSemigroup %2, 1
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc {void*, {void*, %a*}*, %f*}* @callClosurefmap({void*, %Functor*}* %closure){
entry:
  %0 = getelementptr {void*, %Functor*}, {void*, %Functor*}* %closure, i32 0, i32 1
  %1 = load %Functor*, %Functor** %0, align 8
  %2 = call ccc {void*, {void*, %a*}*, %f*}* @fmap(%Functor* %1)
  ret {void*, {void*, %a*}*, %f*}* %2
}

define external ccc {void*, {void*, %a*}*, %f*}* @fmap(%Functor* %x1){
entry:
  %0 = load %Functor, %Functor* %x1, align 8
  %1 = extractvalue %Functor %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Functor %0 to %MkFunctor
  %3 = extractvalue %MkFunctor %2, 1
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %String* @callClosureshow({void*, %Show*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Show*, %a*}, {void*, %Show*, %a*}* %closure, i32 0, i32 1
  %1 = load %Show*, %Show** %0, align 8
  %2 = getelementptr {void*, %Show*, %a*}, {void*, %Show*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = call ccc %String* @show(%a* %3, %Show* %1)
  ret %String* %4
}

define external ccc %String* @show(%Show* %x1){
entry:
  %0 = load %Show, %Show* %x1, align 8
  %1 = extractvalue %Show %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Show %0 to %MkShow
  %3 = extractvalue %MkShow %2, 1
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %Bool* @callClosureeq({void*, %Eq*, %a*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Eq*, %a*, %a*}, {void*, %Eq*, %a*, %a*}* %closure, i32 0, i32 1
  %1 = load %Eq*, %Eq** %0, align 8
  %2 = getelementptr {void*, %Eq*, %a*, %a*}, {void*, %Eq*, %a*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = getelementptr {void*, %Eq*, %a*, %a*}, {void*, %Eq*, %a*, %a*}* %closure, i32 0, i32 3
  %5 = load %a*, %a** %4, align 8
  %6 = call ccc %Bool* @eq(%a* %5, %a* %3, %Eq* %1)
  ret %Bool* %6
}

define external ccc %Bool* @eq(%Eq* %x1){
entry:
  %0 = load %Eq, %Eq* %x1, align 8
  %1 = extractvalue %Eq %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Eq %0 to %MkEq
  %3 = extractvalue %MkEq %2, 1
  br label %switch_return
trivial_branch:
  %4 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %5 = bitcast void* %4 to {void*}*
  %6 = load {void*}, {void*}* %5, align 8
  %7 = insertvalue {void*} %6, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %8 = phi i64 [5, %branch]
  ret i64 %8
}

define external ccc %Ordering* @callClosurelifted0({void*, %Int*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 2
  %3 = load %Int*, %Int** %2, align 8
  %4 = call ccc %Ordering* @lifted0(%Int* %3, %Int* %1)
  ret %Ordering* %4
}

define external ccc %Ordering* @lifted0(%Int* %x1, %Int* %x2){
entry:
  %0 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*}*
  %2 = load {void*}, {void*}* %1, align 8
  %3 = insertvalue {void*} %2, %Ord* ({void*}*) @callClosureOrdInt, 0
  %4 = call ccc {void*, %a*, %a*}* @lt({void*}* %1)
  %5 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %6 = bitcast void* %5 to {void*}*
  %7 = load {void*}, {void*}* %6, align 8
  %8 = insertvalue {void*} %7, %Ord* ({void*}*) @callClosureOrdInt, 0
  %9 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %4, align 8
  %10 = insertvalue {void*, %a*, %a*} %9, {void*}* %6, 3
  %11 = insertvalue {void*, %a*, %a*} %10, %Int* %x1, 2
  %12 = insertvalue {void*, %a*, %a*} %11, %Int* %x2, 1
  store {void*, %a*, %a*} %12, {void*, %a*, %a*}* %4, align 8
  %13 = extractvalue {void*, %a*, %a*} %9, 0
  %14 = bitcast void* %13 to void* ({void*, %a*, %a*}*)*
  %15 = call ccc void* %14({void*, %a*, %a*}* %4)
  %16 = load void, void* %15, align 8
  %17 = extractvalue void %16, 0
  switch i64 %17, label %default_branch1 [i64 1, label %branch i64 2, label %branch1]
branch:
  %18 = bitcast void %16 to %True
  %19 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %20 = bitcast void* %19 to {void*}*
  %21 = load {void*}, {void*}* %20, align 8
  %22 = insertvalue {void*} %21, %Ordering* ({void*}*) @callClosureLE, 0
  br label %switch_return1
branch1:
  %23 = bitcast void %16 to %False
  %24 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %25 = bitcast void* %24 to {void*}*
  %26 = load {void*}, {void*}* %25, align 8
  %27 = insertvalue {void*} %26, %Ord* ({void*}*) @callClosureOrdInt, 0
  %28 = call ccc {void*, %a*, %a*}* @gt({void*}* %25)
  %29 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %30 = bitcast void* %29 to {void*}*
  %31 = load {void*}, {void*}* %30, align 8
  %32 = insertvalue {void*} %31, %Ord* ({void*}*) @callClosureOrdInt, 0
  %33 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %28, align 8
  %34 = insertvalue {void*, %a*, %a*} %33, {void*}* %30, 3
  %35 = insertvalue {void*, %a*, %a*} %34, %Int* %x1, 2
  %36 = insertvalue {void*, %a*, %a*} %35, %Int* %x2, 1
  store {void*, %a*, %a*} %36, {void*, %a*, %a*}* %28, align 8
  %37 = extractvalue {void*, %a*, %a*} %33, 0
  %38 = bitcast void* %37 to void* ({void*, %a*, %a*}*)*
  %39 = call ccc void* %38({void*, %a*, %a*}* %28)
  %40 = load void, void* %39, align 8
  %41 = extractvalue void %40, 0
  switch i64 %41, label %default_branch [i64 1, label %branch2 i64 2, label %branch3]
branch2:
  %42 = bitcast void %40 to %True
  %43 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %44 = bitcast void* %43 to {void*}*
  %45 = load {void*}, {void*}* %44, align 8
  %46 = insertvalue {void*} %45, %Ordering* ({void*}*) @callClosureGE, 0
  br label %switch_return
branch3:
  %47 = bitcast void %40 to %False
  %48 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %49 = bitcast void* %48 to {void*}*
  %50 = load {void*}, {void*}* %49, align 8
  %51 = insertvalue {void*} %50, %Ordering* ({void*}*) @callClosureEQ, 0
  br label %switch_return
default_branch:
  br label %switch_return
switch_return:
  %52 = phi i64 [5, %branch2], [5, %branch3]
  br label %switch_return1
default_branch1:
  br label %switch_return1
switch_return1:
  %53 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %53
}

define external ccc %Bool* @callClosurelifted1({void*, %Int*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 2
  %3 = load %Int*, %Int** %2, align 8
  %4 = call ccc %Bool* @lifted1(%Int* %3, %Int* %1)
  ret %Bool* %4
}

define external ccc %Bool* @lifted1(%Int* %x1, %Int* %x2){
entry:
  %0 = call ccc %Bool* @gtInt(%Int* %x1, %Int* %x2)
  ret %Bool* %0
}

define external ccc %Bool* @callClosurelifted2({void*, %Bool*, %Bool*}* %closure){
entry:
  %0 = getelementptr {void*, %Bool*, %Bool*}, {void*, %Bool*, %Bool*}* %closure, i32 0, i32 1
  %1 = load %Bool*, %Bool** %0, align 8
  %2 = getelementptr {void*, %Bool*, %Bool*}, {void*, %Bool*, %Bool*}* %closure, i32 0, i32 2
  %3 = load %Bool*, %Bool** %2, align 8
  %4 = call ccc %Bool* @lifted2(%Bool* %3, %Bool* %1)
  ret %Bool* %4
}

define external ccc %Bool* @lifted2(%Bool* %x1, %Bool* %x2){
entry:
  %0 = load %Bool, %Bool* %x1, align 8
  %1 = extractvalue %Bool %0, 0
  switch i64 %1, label %trivial_branch2 [i64 1, label %branch i64 2, label %branch2]
branch:
  %2 = bitcast %Bool %0 to %False
  %3 = load %Bool, %Bool* %x2, align 8
  %4 = extractvalue %Bool %3, 0
  switch i64 %4, label %trivial_branch [i64 1, label %branch1]
branch1:
  %5 = bitcast %Bool %3 to %False
  %6 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %7 = bitcast void* %6 to {void*}*
  %8 = load {void*}, {void*}* %7, align 8
  %9 = insertvalue {void*} %8, %Bool* ({void*}*) @callClosureTrue, 0
  br label %switch_return
trivial_branch:
  %10 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %11 = bitcast void* %10 to {void*}*
  %12 = load {void*}, {void*}* %11, align 8
  %13 = insertvalue {void*} %12, %Bool* ({void*}*) @callClosureFalse, 0
  br label %switch_return
switch_return:
  %14 = phi i64 [5, %branch1]
  br label %switch_return2
branch2:
  %15 = bitcast %Bool %0 to %True
  %16 = load %Bool, %Bool* %x2, align 8
  %17 = extractvalue %Bool %16, 0
  switch i64 %17, label %trivial_branch1 [i64 1, label %branch3]
branch3:
  %18 = bitcast %Bool %16 to %True
  %19 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %20 = bitcast void* %19 to {void*}*
  %21 = load {void*}, {void*}* %20, align 8
  %22 = insertvalue {void*} %21, %Bool* ({void*}*) @callClosureTrue, 0
  br label %switch_return1
trivial_branch1:
  %23 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %24 = bitcast void* %23 to {void*}*
  %25 = load {void*}, {void*}* %24, align 8
  %26 = insertvalue {void*} %25, %Bool* ({void*}*) @callClosureFalse, 0
  br label %switch_return1
switch_return1:
  %27 = phi i64 [5, %branch3]
  br label %switch_return2
trivial_branch2:
  %28 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %29 = bitcast void* %28 to {void*}*
  %30 = load {void*}, {void*}* %29, align 8
  %31 = insertvalue {void*} %30, %Bool* ({void*}*) @callClosureFalse, 0
  br label %switch_return2
switch_return2:
  %32 = phi i64 [5, %branch], [5, %branch2]
  ret i64 %32
}

define external ccc %b* @callClosurelifted3({void*, {void*, %b*, %a*}*, %b*, %Maybe*}* %closure){
entry:
  %0 = getelementptr {void*, {void*, %b*, %a*}*, %b*, %Maybe*}, {void*, {void*, %b*, %a*}*, %b*, %Maybe*}* %closure, i32 0, i32 1
  %1 = load {void*, %b*, %a*}*, {void*, %b*, %a*}** %0, align 8
  %2 = getelementptr {void*, {void*, %b*, %a*}*, %b*, %Maybe*}, {void*, {void*, %b*, %a*}*, %b*, %Maybe*}* %closure, i32 0, i32 2
  %3 = load %b*, %b** %2, align 8
  %4 = getelementptr {void*, {void*, %b*, %a*}*, %b*, %Maybe*}, {void*, {void*, %b*, %a*}*, %b*, %Maybe*}* %closure, i32 0, i32 3
  %5 = load %Maybe*, %Maybe** %4, align 8
  %6 = call ccc %b* @lifted3(%Maybe* %5, %b* %3, {void*, %b*, %a*}* %1)
  ret %b* %6
}

define external ccc %b* @lifted3({void*, %b*, %a*}* %x1, %b* %x2, %Maybe* %x3){
entry:
  %0 = load %Maybe, %Maybe* %x3, align 8
  %1 = extractvalue %Maybe %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Maybe %0 to %Just
  %3 = extractvalue %Just %2, 1
  %4 = load {void*, %b*, %a*}, {void*, %b*, %a*}* %x1, align 8
  %5 = insertvalue {void*, %b*, %a*} %4, %b* %x2, 3
  %6 = insertvalue {void*, %b*, %a*} %5, %a* %3, 2
  store {void*, %b*, %a*} %6, {void*, %b*, %a*}* %x1, align 8
  br label %switch_return
branch1:
  %7 = bitcast %Maybe %0 to %Nothing
  br label %switch_return
trivial_branch:
  %8 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %9 = bitcast void* %8 to {void*}*
  %10 = load {void*}, {void*}* %9, align 8
  %11 = insertvalue {void*} %10, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %12 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %12
}

define external ccc %b* @callClosurelifted4({void*, {void*, %a*, %b*}*, %b*, %Maybe*}* %closure){
entry:
  %0 = getelementptr {void*, {void*, %a*, %b*}*, %b*, %Maybe*}, {void*, {void*, %a*, %b*}*, %b*, %Maybe*}* %closure, i32 0, i32 1
  %1 = load {void*, %a*, %b*}*, {void*, %a*, %b*}** %0, align 8
  %2 = getelementptr {void*, {void*, %a*, %b*}*, %b*, %Maybe*}, {void*, {void*, %a*, %b*}*, %b*, %Maybe*}* %closure, i32 0, i32 2
  %3 = load %b*, %b** %2, align 8
  %4 = getelementptr {void*, {void*, %a*, %b*}*, %b*, %Maybe*}, {void*, {void*, %a*, %b*}*, %b*, %Maybe*}* %closure, i32 0, i32 3
  %5 = load %Maybe*, %Maybe** %4, align 8
  %6 = call ccc %b* @lifted4(%Maybe* %5, %b* %3, {void*, %a*, %b*}* %1)
  ret %b* %6
}

define external ccc %b* @lifted4({void*, %a*, %b*}* %x1, %b* %x2, %Maybe* %x3){
entry:
  %0 = load %Maybe, %Maybe* %x3, align 8
  %1 = extractvalue %Maybe %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Maybe %0 to %Just
  %3 = extractvalue %Just %2, 1
  %4 = load {void*, %a*, %b*}, {void*, %a*, %b*}* %x1, align 8
  %5 = insertvalue {void*, %a*, %b*} %4, %a* %3, 3
  %6 = insertvalue {void*, %a*, %b*} %5, %b* %x2, 2
  store {void*, %a*, %b*} %6, {void*, %a*, %b*}* %x1, align 8
  br label %switch_return
branch1:
  %7 = bitcast %Maybe %0 to %Nothing
  br label %switch_return
trivial_branch:
  %8 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %9 = bitcast void* %8 to {void*}*
  %10 = load {void*}, {void*}* %9, align 8
  %11 = insertvalue {void*} %10, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %12 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %12
}

define external ccc %b* @callClosurelifted5({void*, {void*, %b*, %a*}*, %b*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, {void*, %b*, %a*}*, %b*, %List*}, {void*, {void*, %b*, %a*}*, %b*, %List*}* %closure, i32 0, i32 1
  %1 = load {void*, %b*, %a*}*, {void*, %b*, %a*}** %0, align 8
  %2 = getelementptr {void*, {void*, %b*, %a*}*, %b*, %List*}, {void*, {void*, %b*, %a*}*, %b*, %List*}* %closure, i32 0, i32 2
  %3 = load %b*, %b** %2, align 8
  %4 = getelementptr {void*, {void*, %b*, %a*}*, %b*, %List*}, {void*, {void*, %b*, %a*}*, %b*, %List*}* %closure, i32 0, i32 3
  %5 = load %List*, %List** %4, align 8
  %6 = call ccc %b* @lifted5(%List* %5, %b* %3, {void*, %b*, %a*}* %1)
  ret %b* %6
}

define external ccc %b* @lifted5({void*, %b*, %a*}* %x1, %b* %x2, %List* %x3){
entry:
  %0 = load %List, %List* %x3, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = load {void*, %b*, %a*}, {void*, %b*, %a*}* %x1, align 8
  %6 = insertvalue {void*, %b*, %a*} %5, %b* %x2, 3
  %7 = insertvalue {void*, %b*, %a*} %6, %a* %3, 2
  store {void*, %b*, %a*} %7, {void*, %b*, %a*}* %x1, align 8
  %8 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %9 = bitcast void* %8 to {void*}*
  %10 = load {void*}, {void*}* %9, align 8
  %11 = insertvalue {void*} %10, %Foldable* ({void*}*) @callClosureFoldableList, 0
  %12 = call ccc {void*, {void*, %b*, %a*}*, %b*, %t*}* @foldl({void*}* %9)
  %13 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %14 = bitcast void* %13 to {void*}*
  %15 = load {void*}, {void*}* %14, align 8
  %16 = insertvalue {void*} %15, %Foldable* ({void*}*) @callClosureFoldableList, 0
  %17 = load {void*, {void*, %b*, %a*}*, %b*, %t*}, {void*, {void*, %b*, %a*}*, %b*, %t*}* %12, align 8
  %18 = insertvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %17, {void*}* %14, 4
  %19 = insertvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %18, {void*, %b*, %a*}* %x1, 3
  %20 = insertvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %19, {void*, %b*, %a*}* %x1, 2
  %21 = insertvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %20, %List* %4, 1
  store {void*, {void*, %b*, %a*}*, %b*, %t*} %21, {void*, {void*, %b*, %a*}*, %b*, %t*}* %12, align 8
  %22 = extractvalue {void*, {void*, %b*, %a*}*, %b*, %t*} %17, 0
  %23 = bitcast void* %22 to void* ({void*, {void*, %b*, %a*}*, %b*, %t*}*)*
  %24 = call ccc void* %23({void*, {void*, %b*, %a*}*, %b*, %t*}* %12)
  br label %switch_return
branch1:
  %25 = bitcast %List %0 to %Nil
  br label %switch_return
trivial_branch:
  %26 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %27 = bitcast void* %26 to {void*}*
  %28 = load {void*}, {void*}* %27, align 8
  %29 = insertvalue {void*} %28, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %30 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %30
}

define external ccc %b* @callClosurelifted6({void*, {void*, %a*, %b*}*, %b*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, {void*, %a*, %b*}*, %b*, %List*}, {void*, {void*, %a*, %b*}*, %b*, %List*}* %closure, i32 0, i32 1
  %1 = load {void*, %a*, %b*}*, {void*, %a*, %b*}** %0, align 8
  %2 = getelementptr {void*, {void*, %a*, %b*}*, %b*, %List*}, {void*, {void*, %a*, %b*}*, %b*, %List*}* %closure, i32 0, i32 2
  %3 = load %b*, %b** %2, align 8
  %4 = getelementptr {void*, {void*, %a*, %b*}*, %b*, %List*}, {void*, {void*, %a*, %b*}*, %b*, %List*}* %closure, i32 0, i32 3
  %5 = load %List*, %List** %4, align 8
  %6 = call ccc %b* @lifted6(%List* %5, %b* %3, {void*, %a*, %b*}* %1)
  ret %b* %6
}

define external ccc %b* @lifted6({void*, %a*, %b*}* %x1, %b* %x2, %List* %x3){
entry:
  %0 = load %List, %List* %x3, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %6 = bitcast void* %5 to {void*}*
  %7 = load {void*}, {void*}* %6, align 8
  %8 = insertvalue {void*} %7, %Foldable* ({void*}*) @callClosureFoldableList, 0
  %9 = call ccc {void*, {void*, %a*, %b*}*, %b*, %t*}* @foldr({void*}* %6)
  %10 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %11 = bitcast void* %10 to {void*}*
  %12 = load {void*}, {void*}* %11, align 8
  %13 = insertvalue {void*} %12, %Foldable* ({void*}*) @callClosureFoldableList, 0
  %14 = load {void*, {void*, %a*, %b*}*, %b*, %t*}, {void*, {void*, %a*, %b*}*, %b*, %t*}* %9, align 8
  %15 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %14, {void*}* %11, 4
  %16 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %15, {void*, %a*, %b*}* %x1, 3
  %17 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %16, %b* %x2, 2
  %18 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %17, %List* %4, 1
  store {void*, {void*, %a*, %b*}*, %b*, %t*} %18, {void*, {void*, %a*, %b*}*, %b*, %t*}* %9, align 8
  %19 = extractvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %14, 0
  %20 = bitcast void* %19 to void* ({void*, {void*, %a*, %b*}*, %b*, %t*}*)*
  %21 = call ccc void* %20({void*, {void*, %a*, %b*}*, %b*, %t*}* %9)
  %22 = load {void*, %a*, %b*}, {void*, %a*, %b*}* %x1, align 8
  %23 = insertvalue {void*, %a*, %b*} %22, %a* %3, 3
  %24 = insertvalue {void*, %a*, %b*} %23, void* %21, 2
  store {void*, %a*, %b*} %24, {void*, %a*, %b*}* %x1, align 8
  br label %switch_return
branch1:
  %25 = bitcast %List %0 to %Nil
  br label %switch_return
trivial_branch:
  %26 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %27 = bitcast void* %26 to {void*}*
  %28 = load {void*}, {void*}* %27, align 8
  %29 = insertvalue {void*} %28, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %30 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %30
}

define external ccc %Either* @callClosurelifted7({void*, {void*, %a*}*, %Either*}* %closure){
entry:
  %0 = getelementptr {void*, {void*, %a*}*, %Either*}, {void*, {void*, %a*}*, %Either*}* %closure, i32 0, i32 1
  %1 = load {void*, %a*}*, {void*, %a*}** %0, align 8
  %2 = getelementptr {void*, {void*, %a*}*, %Either*}, {void*, {void*, %a*}*, %Either*}* %closure, i32 0, i32 2
  %3 = load %Either*, %Either** %2, align 8
  %4 = call ccc %Either* @lifted7(%Either* %3, {void*, %a*}* %1)
  ret %Either* %4
}

define external ccc %Either* @lifted7({void*, %a*}* %x1, %Either* %x2){
entry:
  %0 = load %Either, %Either* %x2, align 8
  %1 = extractvalue %Either %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch]
branch:
  %2 = bitcast %Either %0 to %Right
  %3 = extractvalue %Right %2, 1
  %4 = load {void*, %a*}, {void*, %a*}* %x1, align 8
  %5 = insertvalue {void*, %a*} %4, %a* %3, 2
  store {void*, %a*} %5, {void*, %a*}* %x1, align 8
  %6 = call ccc %Either* @Right({void*, %a*}* %x1)
  br label %switch_return1
trivial_branch:
  %7 = load %Either, %Either* %x2, align 8
  %8 = extractvalue %Either %7, 0
  switch i64 %8, label %trivial_branch1 [i64 1, label %branch1]
branch1:
  %9 = bitcast %Either %7 to %Left
  %10 = extractvalue %Left %9, 1
  %11 = call ccc %Either* @Left(%a* %10)
  br label %switch_return
trivial_branch1:
  %12 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %13 = bitcast void* %12 to {void*}*
  %14 = load {void*}, {void*}* %13, align 8
  %15 = insertvalue {void*} %14, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %16 = phi i64 [5, %branch1]
  br label %switch_return1
switch_return1:
  %17 = phi i64 [5, %branch]
  ret i64 %17
}

define external ccc %List* @callClosurelifted8({void*, {void*, %a*}*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, {void*, %a*}*, %List*}, {void*, {void*, %a*}*, %List*}* %closure, i32 0, i32 1
  %1 = load {void*, %a*}*, {void*, %a*}** %0, align 8
  %2 = getelementptr {void*, {void*, %a*}*, %List*}, {void*, {void*, %a*}*, %List*}* %closure, i32 0, i32 2
  %3 = load %List*, %List** %2, align 8
  %4 = call ccc %List* @lifted8(%List* %3, {void*, %a*}* %1)
  ret %List* %4
}

define external ccc %List* @lifted8({void*, %a*}* %x1, %List* %x2){
entry:
  %0 = load %List, %List* %x2, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = load {void*, %a*}, {void*, %a*}* %x1, align 8
  %6 = insertvalue {void*, %a*} %5, %a* %3, 2
  store {void*, %a*} %6, {void*, %a*}* %x1, align 8
  %7 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %8 = bitcast void* %7 to {void*}*
  %9 = load {void*}, {void*}* %8, align 8
  %10 = insertvalue {void*} %9, %Functor* ({void*}*) @callClosureFunctorList, 0
  %11 = call ccc {void*, {void*, %a*}*, %f*}* @fmap({void*}* %8)
  %12 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %13 = bitcast void* %12 to {void*}*
  %14 = load {void*}, {void*}* %13, align 8
  %15 = insertvalue {void*} %14, %Functor* ({void*}*) @callClosureFunctorList, 0
  %16 = load {void*, {void*, %a*}*, %f*}, {void*, {void*, %a*}*, %f*}* %11, align 8
  %17 = insertvalue {void*, {void*, %a*}*, %f*} %16, {void*}* %13, 3
  %18 = insertvalue {void*, {void*, %a*}*, %f*} %17, {void*, %a*}* %x1, 2
  %19 = insertvalue {void*, {void*, %a*}*, %f*} %18, %List* %4, 1
  store {void*, {void*, %a*}*, %f*} %19, {void*, {void*, %a*}*, %f*}* %11, align 8
  %20 = extractvalue {void*, {void*, %a*}*, %f*} %16, 0
  %21 = bitcast void* %20 to void* ({void*, {void*, %a*}*, %f*}*)*
  %22 = call ccc void* %21({void*, {void*, %a*}*, %f*}* %11)
  %23 = call ccc %List* @Cons({void*, %a*}* %x1, void* %22)
  br label %switch_return
branch1:
  %24 = bitcast %List %0 to %Nil
  %25 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %26 = bitcast void* %25 to {void*}*
  %27 = load {void*}, {void*}* %26, align 8
  %28 = insertvalue {void*} %27, %List* ({void*}*) @callClosureNil, 0
  br label %switch_return
trivial_branch:
  %29 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %30 = bitcast void* %29 to {void*}*
  %31 = load {void*}, {void*}* %30, align 8
  %32 = insertvalue {void*} %31, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %33 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %33
}

define external ccc %Int* @callClosurelifted9({void*, %Int*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 2
  %3 = load %Int*, %Int** %2, align 8
  %4 = call ccc %Int* @lifted9(%Int* %3, %Int* %1)
  ret %Int* %4
}

define external ccc %Int* @lifted9(%Int* %x1, %Int* %x2){
entry:
  %0 = call ccc %Int* @minusInt(%Int* %x1, %Int* %x2)
  ret %Int* %0
}

define external ccc %List* @callClosurelifted10({void*, %List*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, %List*, %List*}, {void*, %List*, %List*}* %closure, i32 0, i32 1
  %1 = load %List*, %List** %0, align 8
  %2 = getelementptr {void*, %List*, %List*}, {void*, %List*, %List*}* %closure, i32 0, i32 2
  %3 = load %List*, %List** %2, align 8
  %4 = call ccc %List* @lifted10(%List* %3, %List* %1)
  ret %List* %4
}

define external ccc %List* @lifted10(%List* %x1, %List* %x2){
entry:
  %0 = load %List, %List* %x1, align 8
  %1 = extractvalue %List %0, 0
  switch i64 %1, label %trivial_branch1 [i64 1, label %branch i64 2, label %branch2]
branch:
  %2 = bitcast %List %0 to %Cons
  %3 = extractvalue %Cons %2, 1
  %4 = extractvalue %Cons %2, 2
  %5 = load %List, %List* %x2, align 8
  %6 = extractvalue %List %5, 0
  switch i64 %6, label %trivial_branch [i64 1, label %branch1]
branch1:
  %7 = bitcast %List %5 to %Nil
  %8 = call ccc %List* @Cons(%a* %3, %List* %4)
  br label %switch_return
trivial_branch:
  %9 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %10 = bitcast void* %9 to {void*}*
  %11 = load {void*}, {void*}* %10, align 8
  %12 = insertvalue {void*} %11, %Semigroup* ({void*}*) @callClosureSemigroupList_a, 0
  %13 = call ccc {void*, %a*, %a*}* @plus({void*}* %10)
  %14 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %15 = bitcast void* %14 to {void*}*
  %16 = load {void*}, {void*}* %15, align 8
  %17 = insertvalue {void*} %16, %Semigroup* ({void*}*) @callClosureSemigroupList_a, 0
  %18 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %13, align 8
  %19 = insertvalue {void*, %a*, %a*} %18, {void*}* %15, 3
  %20 = insertvalue {void*, %a*, %a*} %19, %List* %4, 2
  %21 = insertvalue {void*, %a*, %a*} %20, %List* %x2, 1
  store {void*, %a*, %a*} %21, {void*, %a*, %a*}* %13, align 8
  %22 = extractvalue {void*, %a*, %a*} %18, 0
  %23 = bitcast void* %22 to void* ({void*, %a*, %a*}*)*
  %24 = call ccc void* %23({void*, %a*, %a*}* %13)
  %25 = call ccc %List* @Cons(%a* %3, void* %24)
  br label %switch_return
switch_return:
  %26 = phi i64 [5, %branch1]
  br label %switch_return1
branch2:
  %27 = bitcast %List %0 to %Nil
  br label %switch_return1
trivial_branch1:
  %28 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %29 = bitcast void* %28 to {void*}*
  %30 = load {void*}, {void*}* %29, align 8
  %31 = insertvalue {void*} %30, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return1
switch_return1:
  %32 = phi i64 [5, %branch], [5, %branch2]
  ret i64 %32
}

define external ccc %Maybe* @callClosurelifted11({void*, %Maybe*, %Maybe*}* %closure){
entry:
  %0 = getelementptr {void*, %Maybe*, %Maybe*}, {void*, %Maybe*, %Maybe*}* %closure, i32 0, i32 1
  %1 = load %Maybe*, %Maybe** %0, align 8
  %2 = getelementptr {void*, %Maybe*, %Maybe*}, {void*, %Maybe*, %Maybe*}* %closure, i32 0, i32 2
  %3 = load %Maybe*, %Maybe** %2, align 8
  %4 = call ccc %Maybe* @lifted11(%Maybe* %3, %Maybe* %1)
  ret %Maybe* %4
}

define external ccc %Maybe* @lifted11(%Maybe* %x1, %Maybe* %x2){
entry:
  %0 = load %Maybe, %Maybe* %x1, align 8
  %1 = extractvalue %Maybe %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Maybe %0 to %Just
  %3 = extractvalue %Just %2, 1
  %4 = call ccc %Maybe* @Just(%a* %3)
  br label %switch_return
branch1:
  %5 = bitcast %Maybe %0 to %Nothing
  br label %switch_return
trivial_branch:
  %6 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %7 = bitcast void* %6 to {void*}*
  %8 = load {void*}, {void*}* %7, align 8
  %9 = insertvalue {void*} %8, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %10 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %10
}

define external ccc %String* @callClosurelifted12({void*, %String*, %String*}* %closure){
entry:
  %0 = getelementptr {void*, %String*, %String*}, {void*, %String*, %String*}* %closure, i32 0, i32 1
  %1 = load %String*, %String** %0, align 8
  %2 = getelementptr {void*, %String*, %String*}, {void*, %String*, %String*}* %closure, i32 0, i32 2
  %3 = load %String*, %String** %2, align 8
  %4 = call ccc %String* @lifted12(%String* %3, %String* %1)
  ret %String* %4
}

define external ccc %String* @lifted12(%String* %x1, %String* %x2){
entry:
  %0 = call ccc %String* @plusStr(%String* %x1, %String* %x2)
  ret %String* %0
}

define external ccc %Int* @callClosurelifted13({void*, %Int*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 2
  %3 = load %Int*, %Int** %2, align 8
  %4 = call ccc %Int* @lifted13(%Int* %3, %Int* %1)
  ret %Int* %4
}

define external ccc %Int* @lifted13(%Int* %x1, %Int* %x2){
entry:
  %0 = call ccc %Int* @plusInt(%Int* %x1, %Int* %x2)
  ret %Int* %0
}

define external ccc %String* @callClosurelifted15({void*, %Show*, %Maybe*}* %closure){
entry:
  %0 = getelementptr {void*, %Show*, %Maybe*}, {void*, %Show*, %Maybe*}* %closure, i32 0, i32 1
  %1 = load %Show*, %Show** %0, align 8
  %2 = getelementptr {void*, %Show*, %Maybe*}, {void*, %Show*, %Maybe*}* %closure, i32 0, i32 2
  %3 = load %Maybe*, %Maybe** %2, align 8
  %4 = call ccc %String* @lifted15(%Maybe* %3, %Show* %1)
  ret %String* %4
}

define external ccc %String* @lifted15(%Show* %cvar14, %Maybe* %x1){
entry:
  %0 = load %Maybe, %Maybe* %x1, align 8
  %1 = extractvalue %Maybe %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Maybe %0 to %Just
  %3 = extractvalue %Just %2, 1
  %4 = call ccc {void*, %a*}* @show(%Show* %cvar14)
  %5 = load {void*, %a*}, {void*, %a*}* %4, align 8
  %6 = insertvalue {void*, %a*} %5, %Show* %cvar14, 2
  %7 = insertvalue {void*, %a*} %6, %a* %3, 1
  store {void*, %a*} %7, {void*, %a*}* %4, align 8
  %8 = extractvalue {void*, %a*} %5, 0
  %9 = bitcast void* %8 to void* ({void*, %a*}*)*
  %10 = call ccc void* %9({void*, %a*}* %4)
  %11 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %12 = bitcast void* %11 to {void*}*
  %13 = load {void*}, {void*}* %12, align 8
  %14 = insertvalue {void*} %13, %Semigroup* ({void*}*) @callClosureSemigroupString, 0
  %15 = call ccc {void*, %a*, %a*}* @plus({void*}* %12)
  %16 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %17 = bitcast void* %16 to {void*}*
  %18 = load {void*}, {void*}* %17, align 8
  %19 = insertvalue {void*} %18, %Semigroup* ({void*}*) @callClosureSemigroupString, 0
  %20 = call ccc %Int* @mkInt(i64 -97)
  %21 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %15, align 8
  %22 = insertvalue {void*, %a*, %a*} %21, {void*}* %17, 3
  %23 = insertvalue {void*, %a*, %a*} %22, %Int* %20, 2
  %24 = insertvalue {void*, %a*, %a*} %23, void* %10, 1
  store {void*, %a*, %a*} %24, {void*, %a*, %a*}* %15, align 8
  %25 = extractvalue {void*, %a*, %a*} %21, 0
  %26 = bitcast void* %25 to void* ({void*, %a*, %a*}*)*
  %27 = call ccc void* %26({void*, %a*, %a*}* %15)
  br label %switch_return
branch1:
  %28 = bitcast %Maybe %0 to %Nothing
  %29 = call ccc %Int* @mkInt(i64 -97)
  br label %switch_return
trivial_branch:
  %30 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %31 = bitcast void* %30 to {void*}*
  %32 = load {void*}, {void*}* %31, align 8
  %33 = insertvalue {void*} %32, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %34 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %34
}

define external ccc %String* @callClosurelifted16({void*, %Bool*}* %closure){
entry:
  %0 = getelementptr {void*, %Bool*}, {void*, %Bool*}* %closure, i32 0, i32 1
  %1 = load %Bool*, %Bool** %0, align 8
  %2 = call ccc %String* @lifted16(%Bool* %1)
  ret %String* %2
}

define external ccc %String* @lifted16(%Bool* %x1){
entry:
  %0 = load %Bool, %Bool* %x1, align 8
  %1 = extractvalue %Bool %0, 0
  switch i64 %1, label %trivial_branch [i64 1, label %branch i64 2, label %branch1]
branch:
  %2 = bitcast %Bool %0 to %False
  %3 = call ccc %Int* @mkInt(i64 -97)
  br label %switch_return
branch1:
  %4 = bitcast %Bool %0 to %True
  %5 = call ccc %Int* @mkInt(i64 -97)
  br label %switch_return
trivial_branch:
  %6 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %7 = bitcast void* %6 to {void*}*
  %8 = load {void*}, {void*}* %7, align 8
  %9 = insertvalue {void*} %8, %a* ({void*}*) @callClosurefailedPattern, 0
  br label %switch_return
switch_return:
  %10 = phi i64 [5, %branch], [5, %branch1]
  ret i64 %10
}

define external ccc %String* @callClosurelifted17({void*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*}, {void*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = call ccc %String* @lifted17(%Int* %1)
  ret %String* %2
}

define external ccc %String* @lifted17(%Int* %x1){
entry:
  %0 = call ccc %String* @showInt(%Int* %x1)
  ret %String* %0
}

define external ccc %String* @callClosurelifted19({void*, %Show*, %List*}* %closure){
entry:
  %0 = getelementptr {void*, %Show*, %List*}, {void*, %Show*, %List*}* %closure, i32 0, i32 1
  %1 = load %Show*, %Show** %0, align 8
  %2 = getelementptr {void*, %Show*, %List*}, {void*, %Show*, %List*}* %closure, i32 0, i32 2
  %3 = load %List*, %List** %2, align 8
  %4 = call ccc %String* @lifted19(%List* %3, %Show* %1)
  ret %String* %4
}

define external ccc %String* @lifted19(%Show* %cvar18, %List* %x1){
entry:
  %0 = call ccc {void*, %a*}* @show(%Show* %cvar18)
  %1 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %2 = bitcast void* %1 to {void*}*
  %3 = load {void*}, {void*}* %2, align 8
  %4 = insertvalue {void*} %3, %Functor* ({void*}*) @callClosureFunctorList, 0
  %5 = call ccc {void*, {void*, %a*}*, %f*}* @fmap({void*}* %2)
  %6 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %7 = bitcast void* %6 to {void*}*
  %8 = load {void*}, {void*}* %7, align 8
  %9 = insertvalue {void*} %8, %Functor* ({void*}*) @callClosureFunctorList, 0
  %10 = load {void*, {void*, %a*}*, %f*}, {void*, {void*, %a*}*, %f*}* %5, align 8
  %11 = insertvalue {void*, {void*, %a*}*, %f*} %10, {void*}* %7, 3
  %12 = insertvalue {void*, {void*, %a*}*, %f*} %11, {void*, %a*}* %0, 2
  %13 = insertvalue {void*, {void*, %a*}*, %f*} %12, %List* %x1, 1
  store {void*, {void*, %a*}*, %f*} %13, {void*, {void*, %a*}*, %f*}* %5, align 8
  %14 = extractvalue {void*, {void*, %a*}*, %f*} %10, 0
  %15 = bitcast void* %14 to void* ({void*, {void*, %a*}*, %f*}*)*
  %16 = call ccc void* %15({void*, {void*, %a*}*, %f*}* %5)
  %17 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %18 = bitcast void* %17 to {void*}*
  %19 = load {void*}, {void*}* %18, align 8
  %20 = insertvalue {void*} %19, %Semigroup* ({void*}*) @callClosureSemigroupString, 0
  %21 = call ccc {void*, %a*, %a*}* @plus({void*}* %18)
  %22 = call ccc %Int* @mkInt(i64 -97)
  %23 = call ccc %List* @intersperse(%Int* %22, void* %16)
  %24 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %25 = bitcast void* %24 to {void*}*
  %26 = load {void*}, {void*}* %25, align 8
  %27 = insertvalue {void*} %26, %Foldable* ({void*}*) @callClosureFoldableList, 0
  %28 = call ccc {void*, {void*, %a*, %b*}*, %b*, %t*}* @foldr({void*}* %25)
  %29 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %30 = bitcast void* %29 to {void*}*
  %31 = load {void*}, {void*}* %30, align 8
  %32 = insertvalue {void*} %31, %Foldable* ({void*}*) @callClosureFoldableList, 0
  %33 = call ccc %Int* @mkInt(i64 -97)
  %34 = load {void*, {void*, %a*, %b*}*, %b*, %t*}, {void*, {void*, %a*, %b*}*, %b*, %t*}* %28, align 8
  %35 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %34, {void*}* %30, 4
  %36 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %35, {void*, %a*, %a*}* %21, 3
  %37 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %36, %Int* %33, 2
  %38 = insertvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %37, %List* %23, 1
  store {void*, {void*, %a*, %b*}*, %b*, %t*} %38, {void*, {void*, %a*, %b*}*, %b*, %t*}* %28, align 8
  %39 = extractvalue {void*, {void*, %a*, %b*}*, %b*, %t*} %34, 0
  %40 = bitcast void* %39 to void* ({void*, {void*, %a*, %b*}*, %b*, %t*}*)*
  %41 = call ccc void* %40({void*, {void*, %a*, %b*}*, %b*, %t*}* %28)
  %42 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %43 = bitcast void* %42 to {void*}*
  %44 = load {void*}, {void*}* %43, align 8
  %45 = insertvalue {void*} %44, %Semigroup* ({void*}*) @callClosureSemigroupString, 0
  %46 = call ccc {void*, %a*, %a*}* @plus({void*}* %43)
  %47 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %48 = bitcast void* %47 to {void*}*
  %49 = load {void*}, {void*}* %48, align 8
  %50 = insertvalue {void*} %49, %Semigroup* ({void*}*) @callClosureSemigroupString, 0
  %51 = call ccc %Int* @mkInt(i64 -97)
  %52 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %46, align 8
  %53 = insertvalue {void*, %a*, %a*} %52, {void*}* %48, 3
  %54 = insertvalue {void*, %a*, %a*} %53, %Int* %51, 2
  %55 = insertvalue {void*, %a*, %a*} %54, void* %41, 1
  store {void*, %a*, %a*} %55, {void*, %a*, %a*}* %46, align 8
  %56 = extractvalue {void*, %a*, %a*} %52, 0
  %57 = bitcast void* %56 to void* ({void*, %a*, %a*}*)*
  %58 = call ccc void* %57({void*, %a*, %a*}* %46)
  %59 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %60 = bitcast void* %59 to {void*}*
  %61 = load {void*}, {void*}* %60, align 8
  %62 = insertvalue {void*} %61, %Semigroup* ({void*}*) @callClosureSemigroupString, 0
  %63 = call ccc {void*, %a*, %a*}* @plus({void*}* %60)
  %64 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %65 = bitcast void* %64 to {void*}*
  %66 = load {void*}, {void*}* %65, align 8
  %67 = insertvalue {void*} %66, %Semigroup* ({void*}*) @callClosureSemigroupString, 0
  %68 = call ccc %Int* @mkInt(i64 -97)
  %69 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %63, align 8
  %70 = insertvalue {void*, %a*, %a*} %69, {void*}* %65, 3
  %71 = insertvalue {void*, %a*, %a*} %70, void* %58, 2
  %72 = insertvalue {void*, %a*, %a*} %71, %Int* %68, 1
  store {void*, %a*, %a*} %72, {void*, %a*, %a*}* %63, align 8
  %73 = extractvalue {void*, %a*, %a*} %69, 0
  %74 = bitcast void* %73 to void* ({void*, %a*, %a*}*)*
  %75 = call ccc void* %74({void*, %a*, %a*}* %63)
  ret void* %75
}

define external ccc %a* @callClosurelifted21({void*, %a*, %b*}* %closure){
entry:
  %0 = getelementptr {void*, %a*, %b*}, {void*, %a*, %b*}* %closure, i32 0, i32 1
  %1 = load %a*, %a** %0, align 8
  %2 = getelementptr {void*, %a*, %b*}, {void*, %a*, %b*}* %closure, i32 0, i32 2
  %3 = load %b*, %b** %2, align 8
  %4 = call ccc %a* @lifted21(%b* %3, %a* %1)
  ret %a* %4
}

define external ccc %a* @lifted21(%a* %cvar20, %b* %x){
entry:
  ret %a* %cvar20
}

define external ccc %Int* @callClosurelifted22({void*, %Int*, %a*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*, %a*}, {void*, %Int*, %a*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = getelementptr {void*, %Int*, %a*}, {void*, %Int*, %a*}* %closure, i32 0, i32 2
  %3 = load %a*, %a** %2, align 8
  %4 = call ccc %Int* @lifted22(%a* %3, %Int* %1)
  ret %Int* %4
}

define external ccc %Int* @lifted22(%Int* %x, %a* %y){
entry:
  %0 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*}*
  %2 = load {void*}, {void*}* %1, align 8
  %3 = insertvalue {void*} %2, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %4 = call ccc {void*, %a*, %a*}* @plus({void*}* %1)
  %5 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %6 = bitcast void* %5 to {void*}*
  %7 = load {void*}, {void*}* %6, align 8
  %8 = insertvalue {void*} %7, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %9 = call ccc %Int* @mkInt(i64 1)
  %10 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %4, align 8
  %11 = insertvalue {void*, %a*, %a*} %10, {void*}* %6, 3
  %12 = insertvalue {void*, %a*, %a*} %11, %Int* %x, 2
  %13 = insertvalue {void*, %a*, %a*} %12, %Int* %9, 1
  store {void*, %a*, %a*} %13, {void*, %a*, %a*}* %4, align 8
  %14 = extractvalue {void*, %a*, %a*} %10, 0
  %15 = bitcast void* %14 to void* ({void*, %a*, %a*}*)*
  %16 = call ccc void* %15({void*, %a*, %a*}* %4)
  ret void* %16
}

define external ccc %Int* @callClosurelifted24({void*, %Int*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = getelementptr {void*, %Int*, %Int*}, {void*, %Int*, %Int*}* %closure, i32 0, i32 2
  %3 = load %Int*, %Int** %2, align 8
  %4 = call ccc %Int* @lifted24(%Int* %3, %Int* %1)
  ret %Int* %4
}

define external ccc %Int* @lifted24(%Int* %cvar23, %Int* %y){
entry:
  %0 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*}*
  %2 = load {void*}, {void*}* %1, align 8
  %3 = insertvalue {void*} %2, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %4 = call ccc {void*, %a*, %a*}* @plus({void*}* %1)
  %5 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %6 = bitcast void* %5 to {void*}*
  %7 = load {void*}, {void*}* %6, align 8
  %8 = insertvalue {void*} %7, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %9 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %4, align 8
  %10 = insertvalue {void*, %a*, %a*} %9, {void*}* %6, 3
  %11 = insertvalue {void*, %a*, %a*} %10, %Int* %cvar23, 2
  %12 = insertvalue {void*, %a*, %a*} %11, %Int* %y, 1
  store {void*, %a*, %a*} %12, {void*, %a*, %a*}* %4, align 8
  %13 = extractvalue {void*, %a*, %a*} %9, 0
  %14 = bitcast void* %13 to void* ({void*, %a*, %a*}*)*
  %15 = call ccc void* %14({void*, %a*, %a*}* %4)
  ret void* %15
}

define external ccc %Int* @callClosurelifted28({void*, %Int*, %Int*, %Int*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = getelementptr {void*, %Int*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*, %Int*}* %closure, i32 0, i32 2
  %3 = load %Int*, %Int** %2, align 8
  %4 = getelementptr {void*, %Int*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*, %Int*}* %closure, i32 0, i32 3
  %5 = load %Int*, %Int** %4, align 8
  %6 = getelementptr {void*, %Int*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*, %Int*}* %closure, i32 0, i32 4
  %7 = load %Int*, %Int** %6, align 8
  %8 = call ccc %Int* @lifted28(%Int* %7, %Int* %5, %Int* %3, %Int* %1)
  ret %Int* %8
}

define external ccc %Int* @lifted28(%Int* %cvar25, %Int* %cvar26, %Int* %cvar27, %Int* %b){
entry:
  %0 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %1 = bitcast void* %0 to {void*}*
  %2 = load {void*}, {void*}* %1, align 8
  %3 = insertvalue {void*} %2, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %4 = call ccc {void*, %a*, %a*}* @plus({void*}* %1)
  %5 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %6 = bitcast void* %5 to {void*}*
  %7 = load {void*}, {void*}* %6, align 8
  %8 = insertvalue {void*} %7, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %9 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %4, align 8
  %10 = insertvalue {void*, %a*, %a*} %9, {void*}* %6, 3
  %11 = insertvalue {void*, %a*, %a*} %10, %Int* %cvar25, 2
  %12 = insertvalue {void*, %a*, %a*} %11, %Int* %b, 1
  store {void*, %a*, %a*} %12, {void*, %a*, %a*}* %4, align 8
  %13 = extractvalue {void*, %a*, %a*} %9, 0
  %14 = bitcast void* %13 to void* ({void*, %a*, %a*}*)*
  %15 = call ccc void* %14({void*, %a*, %a*}* %4)
  %16 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %17 = bitcast void* %16 to {void*}*
  %18 = load {void*}, {void*}* %17, align 8
  %19 = insertvalue {void*} %18, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %20 = call ccc {void*, %a*, %a*}* @plus({void*}* %17)
  %21 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %22 = bitcast void* %21 to {void*}*
  %23 = load {void*}, {void*}* %22, align 8
  %24 = insertvalue {void*} %23, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %25 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %20, align 8
  %26 = insertvalue {void*, %a*, %a*} %25, {void*}* %22, 3
  %27 = insertvalue {void*, %a*, %a*} %26, void* %15, 2
  %28 = insertvalue {void*, %a*, %a*} %27, %Int* %cvar26, 1
  store {void*, %a*, %a*} %28, {void*, %a*, %a*}* %20, align 8
  %29 = extractvalue {void*, %a*, %a*} %25, 0
  %30 = bitcast void* %29 to void* ({void*, %a*, %a*}*)*
  %31 = call ccc void* %30({void*, %a*, %a*}* %20)
  %32 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %33 = bitcast void* %32 to {void*}*
  %34 = load {void*}, {void*}* %33, align 8
  %35 = insertvalue {void*} %34, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %36 = call ccc {void*, %a*, %a*}* @plus({void*}* %33)
  %37 = call ccc void* %malloc({void*}* getelementptr ({void*}, {void*}* zeroinitializer, i32 1))
  %38 = bitcast void* %37 to {void*}*
  %39 = load {void*}, {void*}* %38, align 8
  %40 = insertvalue {void*} %39, %Semigroup* ({void*}*) @callClosureSemigroupInt, 0
  %41 = load {void*, %a*, %a*}, {void*, %a*, %a*}* %36, align 8
  %42 = insertvalue {void*, %a*, %a*} %41, {void*}* %38, 3
  %43 = insertvalue {void*, %a*, %a*} %42, void* %31, 2
  %44 = insertvalue {void*, %a*, %a*} %43, %Int* %cvar27, 1
  store {void*, %a*, %a*} %44, {void*, %a*, %a*}* %36, align 8
  %45 = extractvalue {void*, %a*, %a*} %41, 0
  %46 = bitcast void* %45 to void* ({void*, %a*, %a*}*)*
  %47 = call ccc void* %46({void*, %a*, %a*}* %36)
  ret void* %47
}

define external ccc %Int* @callClosurelifted30({void*, %Int*, %Int*, %Int*}* %closure){
entry:
  %0 = getelementptr {void*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*}* %closure, i32 0, i32 1
  %1 = load %Int*, %Int** %0, align 8
  %2 = getelementptr {void*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*}* %closure, i32 0, i32 2
  %3 = load %Int*, %Int** %2, align 8
  %4 = getelementptr {void*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*}* %closure, i32 0, i32 3
  %5 = load %Int*, %Int** %4, align 8
  %6 = call ccc %Int* @lifted30(%Int* %5, %Int* %3, %Int* %1)
  ret %Int* %6
}

define external ccc %Int* @lifted30(%Int* %cvar29, %Int* %a){
entry:
  %0 = call ccc %Int* @mkInt(i64 2)
  %1 = call ccc void* %malloc({void*, %Int*, %Int*, %Int*, %Int*}* getelementptr ({void*, %Int*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*, %Int*}* zeroinitializer, i32 1))
  %2 = bitcast void* %1 to {void*, %Int*, %Int*, %Int*, %Int*}*
  %3 = load {void*, %Int*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*, %Int*}* %2, align 8
  %4 = insertvalue {void*, %Int*, %Int*, %Int*, %Int*} %3, %Int* ({void*, %Int*, %Int*, %Int*, %Int*}*) @callClosurelifted28, 0
  %5 = load {void*, %Int*, %Int*, %Int*, %Int*}, {void*, %Int*, %Int*, %Int*, %Int*}* %2, align 8
  %6 = insertvalue {void*, %Int*, %Int*, %Int*, %Int*} %5, %Int* %a, 5
  %7 = insertvalue {void*, %Int*, %Int*, %Int*, %Int*} %6, %Int* %cvar29, 4
  %8 = insertvalue {void*, %Int*, %Int*, %Int*, %Int*} %7, %Int* %0, 3
  store {void*, %Int*, %Int*, %Int*, %Int*} %8, {void*, %Int*, %Int*, %Int*, %Int*}* %2, align 8
  ret {void*, %Int*, %Int*, %Int*, %Int*}* %2
}

%Monad = type opaque

%Applicative = type opaque

%Bounded = type opaque

%Enum = type opaque

%Foldable = type opaque

%Ord = type opaque

%Group = type opaque

%Monoid = type opaque

%Semigroup = type opaque

%Functor = type opaque

%Show = type opaque

%Eq = type opaque

%Bool = type opaque

%Either = type opaque

%List = type opaque

%Maybe = type opaque

%Ordering = type opaque

%Tuple = type opaque

%Unit = type opaque

%Int = type opaque
%Double = type opaque

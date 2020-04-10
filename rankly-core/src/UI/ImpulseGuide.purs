module UI.ImpulseGuide where

import Prelude
import App
import Data.Maybe
import Data.String as S
import Data.Symbol
import Impulse.DOM
import Impulse.FRP as FRP
import Effect.Unsafe

p_lang = SProxy :: SProxy "lang"
p_counter = SProxy :: SProxy "counter"
p_a = SProxy :: SProxy "a"
s_timer = (unsafePerformEffect $ FRP.eff_sigBuilder $ FRP.s_reduce_e (\count _ -> count + 1) 0 (FRP.timer 1000)).signal

data Lang = PS | JS

instance eqLang :: Eq Lang where
  eq PS PS = true
  eq JS JS = true
  eq _ _ = false

renderSection ::
  forall e c.
  { title :: Maybe String
  , preCode :: AppDOM e c Unit
  , postCode :: AppDOM e c Unit
  , code :: AppDOM ( lang :: FRP.Signal Lang | e ) ( lang :: Collector Lang | c ) Unit
  , js :: String
  , ps :: String
  } ->
  AppDOM e c Unit
renderSection { title, preCode, postCode, code, js, ps } = do
  keyed (show title) do
    case title of
      Nothing -> dnil
      Just title -> h2_ anil $ text title
    preCode
    e_collectAndReduce p_lang (\_ next -> next) JS do
      s_lang <- getEnv p_lang
      div_ (className "card columns") do
        div_ (className "column is-three-quarters") do
          div_ anil do
            s_bindDOM_ s_lang \lang -> do
              div_ (className "field has-addons") do
                p_ (className "control") $ a_ (className "button is-small is-static") $ text "code"
                b_js <- p (className "control") $ button_ (classNames do
                                                             cn "button is-small"
                                                             whenM (pure $ lang == JS) $ cn "is-active") $ text "javascript"
                b_ps <- p (className "control") $ button_ (classNames do
                                                             cn "button is-small"
                                                             whenM (pure $ lang == PS) $ cn "is-active") $ text "purescript"
                e_emit p_lang $ onClick b_js <#> const JS
                e_emit p_lang $ onClick b_ps <#> const PS
          pre_ anil do
            s_bindDOM_ s_lang \lang -> case lang of
              JS -> code_ (className "language-javascript") $ text $ S.drop 1 js
              PS -> code_ (className "language-haskell") $ text $ S.drop 1 ps
        div_ (className "column") do
          div_ (className "field has-addons") do
            p_ (className "control") $ a_ (className "button is-small is-static") $ text "demo"
          div_ anil $ keyed "codeSection" code
    postCode

impulseGuide :: forall e c. AppDOM e c Unit
impulseGuide = do
  div_ (className "container") $ div_ (className "content") do
    h1_ anil $ text "Gentle Introduction to Impulse for React developers"
    p_ anil $ text "Impulse uses a monadic interface to create components/UIs. For now we will skip the Category Theory behind what that actually means and approach the available API from a practical point of view."
    p_ anil $ text "I tend to think of there being 2 dimensions of computation when building complex UIs. Those 2 dimensions being:"
    ol_ anil $ do
      li_ anil $ text "Building up the markup that represents the current view of the application"
      li_ anil $ text "State management"
    p_ anil $ text "In UI frameworks like React the primary dimension that our functions calculate is the 1st dimension evidenced by the fact that our most basic components are functions that return markup. This forces us to handle the secondary dimension of state management in less composable ways like in class methods and higher order components, essentially as side effects. To me this is not the best solution because functions are out greatest tool in programming but with this paradigm the values returned from our functions (ie the markup) are the lesser complex of the two dimensions."
    p_ anil $ text "In Impulse we try to invert this paradigm by allowing our function components to return whatever they want and the building up of markup is treated as a side effect of our computation."
    p_ anil do
      text "Let's look at examples of the API to make this point more clear. Each of the examples will have the purescript implementation as well as a rough javascript translation of the purescript code. This isn't the actual compiled javascript and writing it would not be compatible with other Impulse components, but it is meant to get the idea across. In the javascript examples, a component is defined as a function from "
      span_ (className "pre") $ text "I"
      text " to any return value. This "
      span_ (className "pre") $ text "I"
      text " value is maintained by the framework and is used to store and maintain state. In purescript, this object is maintained under the hood and a component is anything that has the type: "
      span_ (className "pre") $ text "DOM e c a"
      text " (more on what that type means later)."
    renderSection { title: Just "text(s)"
                  , preCode: p_ anil do
                               span_ (className "pre") $ text "text"
                               text " is the simplest function that renders content to the page. It simply plops the supplied string in the current place in DOM. This shows the difference between React already though because you can see our component is not returning a fragment containing a bunch of text, we are making repeated calls to the "
                               span_ (className "pre") $ text "text"
                               text " function which draws the string on the page as a side effect. The "
                               span_ (className "pre") $ text "text"
                               text " function has no return value."
                  , postCode: dnil
                  , code: do text "Hel"
                             text "lo "
                             text "wor"
                             text "ld!"
                  , js: """
const ui1 = (I) => {
	I.text("Hel");
	I.text("lo ";
	I.text("wor");
	I.text("ld!");
};
"""
                  , ps: """
ui1 :: forall e c. DOM e c Unit
ui1 = do
	text "Hel"
	text "lo "
	text "wor"
	text "ld!"
"""
                  }
    renderSection { title: Just "getEnv(prop) => val / upsertEnv(prop, val, drawChildren)"
                  , preCode: do p_ anil do
                                  text "Impulse maintains a readonly environment. It can be accessed with "
                                  span_ (className "pre") $ text "getEnv"
                                  text " and while the current environment can not be modified, we can run other components inside a modified environment using "
                                  span_ (className "pre") $ text "upsertEnv"
                                  text "."
                  , postCode: dnil
                  , code: do upsertEnv p_a 1 do upsertEnv p_a 2 do a <- getEnv p_a
                                                                   text $ "Inner val: " <> (show a) <> ". "
                                                a <- getEnv p_a
                                                text $ "Outer val: " <> (show a) <> "."
                  , js: """
const ui2 = (I) => {
	I.upsertEnv('a', 1, (I) => {
		I.upsertEnv('a', 2, (I) => {
			const a = I.getEnv('a');
			I.text(`Inner val: ${a}. `);
		});
		const a = I.getEnv('a');
		I.text(`Outer val: ${a}.`);
	});
};
"""
                  , ps: """
p_a = SProxy :: SProxy "a"

ui2 :: forall e c. DOM e c Unit
ui2 = do
	upsertEnv p_a 1 do
		upsertEnv p_a 2 do
			a <- getEnv p_a
			text $ "Inner val: " <> (show a) <> ". "
		a <- getEnv p_a
		text $ "Outer val: " <> (show a) <> "."
"""
                  }
    renderSection { title: Just "s_bindDOM(signal, drawChildrenFromCurrentVal)"
                  , preCode: do p_ anil do
                                  text "Impulse uses a system of "
                                  span_ (className "pre") $ text "Event"
                                  text "s and "
                                  span_ (className "pre") $ text "Signal"
                                  text "s to manage state. An event represents a value that exists at certain discrete times and a signal represents a continuous value that changes over time. In this example we create a Signal that increments its value once every second. Then we use "
                                  span_ (className "pre") $ text "s_bindDOM"
                                  text " which allows us to bind UI that updates every time the signal changes, receiving its current value as a function parameter."
                  , postCode: dnil
                  , code: do s_bindDOM_ s_timer \timer -> text $ show timer
                             text " second(s) elapsed"
                  , js: """
const s_timer = frp.reduce(count => count + 1, 0, frp.timer(1000));
const ui3 = (I) => {
	I.s_bindDOM(s_timer, (timer) => (I) => {
		I.text(`${timer}`);
	});
	I.text(" second(s) elapsed.");
};
"""
                  , ps: """
s_timer = (unsafePerformEffect $ FRP.eff_sigBuilder $ FRP.s_reduce_e (\count _ -> count + 1) 0 (FRP.timer 1000)).signal

ui3 :: forall e c. DOM e c Unit
ui3 = do
	s_bindDOM_ s_timer \timer -> text $ show timer
	text " second(s) elapsed"
"""
                  }
    renderSection { title: Just "s_use(signalBuilder) => signal"
                  , preCode: do p_ anil do
                                  span_ (className "pre") $ text "s_use"
                                  text " lets you create a signal whose lifetime is tied to the component it is used in. Looking at this example very similar to the above:"
                  , postCode: do p_ anil do
                                   text "The key difference is this timer will always show the time since the component was mounted compared to the global timer in the previous example. In this example the signal gets created on mount, garbage collected on unmount and rebuilt on future remounts. In the previous example the signal would keep updating as long as the application was running (unless some action was taken to explicitly shut if off) even if the component was not mounted or unmounted."
                  , code: do s_timer <- s_use $ FRP.s_reduce_e (\count _ -> count + 1) 0 (FRP.timer 1000)
                             s_bindDOM_ s_timer \timer -> text $ show timer
                             text " second(s) elapsed"
                  , js: """
const ui4 = (I) => {
	const s_timer = I.s_use(
		(S) => S.reduce_e(count => count + 1, 0, frp.timer(1000))
	);
	I.s_bindDOM(s_timer, (timer) => (I) => {
		I.text(`${timer}`);
	});
	I.text(" second(s) elapsed.");
};
"""
                  , ps: """
ui4 :: forall e c. DOM e c Unit
ui4 = do
	s_timer <- s_use $ FRP.s_reduce_e (\count _ -> count + 1) 0 (FRP.timer 1000)
	s_bindDOM_ s_timer \timer -> text $ show timer
	text " second(s) elapsed"
"""
                  }
    renderSection { title: Nothing
                  , preCode: do p_ anil do
                                  span_ (className "pre") $ text "s_use"
                                  text " allows you to chain several signal manipulating actions together, and the framework will bundle all of their shutdown handlers into one function that it calls on unmount."
                  , postCode: dnil
                  , code: do s_timer <- s_use $ FRP.s_reduce_e (\count _ -> count + 1) 0 (FRP.timer 1000)
                             s_timerHalfTime <- s_use do
                               s_odds <- FRP.s_filter (\i -> i `mod` 2 == 1) s_timer
                               FRP.s_fmap (\i -> i / 2) s_odds
                             s_bindDOM_ s_timer \timer -> text $ show timer
                             text " tick(s) normal speed. "
                             s_bindDOM_ s_timerHalfTime \timerHalfTime -> text $ show timerHalfTime
                             text " tick(s) half speed."
                  , js: """
const ui5 = (I) => {
	const s_timer = I.s_use(
		(S) => S.reduce_e(count => count + 1, 0, frp.timer(1000))
	);
	const s_timerHalfTime = I.s_use(
		(S) => {
			// s_odds will be automatically garbage collected
			// at the same time as s_timerHalfTime
			const s_odds = S.filter(i => i % 2 === 1, s_timer);
			return S.fmap(i => i / 2, s_odds);
		}
	);
	I.s_bindDOM(s_timer, (timer) => (I) => {
		I.text(`${timer}`);
	});
	I.text(" tick(s) normal speed. ");
	I.s_bindDOM(s_timerHalfTime, (timerHalfTime) => (I) => {
		I.text(`${timerHalfTime}`);
	});
	I.text(" tick(s) half speed.");
};
"""
                  , ps: """
ui5 :: forall e c. DOM e c Unit
ui5 = do
	s_timer <- s_use $ FRP.s_reduce_e (\count _ -> count + 1) 0 (FRP.timer 1000)
	s_timerHalfTime <- s_use do
		-- s_odds will be automatically garbage collected
		-- at the same time as s_timerHalfTime
		s_odds <- FRP.s_filter (\i -> i `mod` 2 == 1) s_timer
		FRP.s_fmap (\i -> i / 2) s_odds
	s_bindDOM_ s_timer \timer -> text $ show timer
	text " tick(s) normal speed. "
	s_bindDOM_ s_timerHalfTime \timerHalfTime -> text $ show timerHalfTime
	text " tick(s) half speed."
"""
                  }
    renderSection { title: Just "createElement (tag, attrs, drawChildren) => ImpulseEl"
                  , preCode: do p_ anil do
                                  text "Draws a DOM element "
                                  span_ (className "pre") $ text "tag"
                                  text " in the next place in the DOM with its children defined by "
                                  span_ (className "pre") $ text "drawChildren"
                                  text " being a function from "
                                  span_ (className "pre") $ text "impulseContext"
                                  text " to any value. The return value is an "
                                  span_ (className "pre") $ text "ImpulseEl"
                                  text " which contains a set of events for the drawn DOM element (ie "
                                  span_ (className "pre") $ text "onClick"
                                  text ", "
                                  span_ (className "pre") $ text "onHover"
                                  text ", "
                                  span_ (className "pre") $ text "onChange"
                                  text " etc.) as well as the return value of the "
                                  span_ (className "pre") $ text "drawChildren"
                                  text " function."
                                p_ anil do
                                  text "All tags have a corresponding function that is more succinct version of "
                                  span_ (className "pre") $ text "createElement"
                                  text " ie:"
                                pre_ anil do
                                  code_ (className "language-javascript") $ text """I.button = (attrs, drawChildren) => I.createElement('button', attrs, drawChildren);
I.div = (attrs, drawChildren) => I.createElement('div', attrs, drawChildren);
// ... etc
"""
                                p_ anil do
                                  text "We can use the events returned by "
                                  span_ (className "pre") $ text "createElement"
                                  text " to build and bind to signals."
                  , postCode: p_ anil $ text """This is the first we are seeing of our UI components being able to return values but it is a very powerful concept and is what allows us to build truly reusable components that do not need to rely on additional 3rd party state management solutions. For example consider a Color Picker component that allows a user to drag a selector around a color wheel to pick the color they want. A well designed implementation of this in Impulse would draw the color picker on screen and return the signal representing the current selected color as well as an event representing any time the user hit enter while the picker was in focus or they clicked an "accept" button that the component rendered. This will be easy to slot into any impulse application because we can just use simple functions to transform and filter the returned signal and event to fit our specific types and use case."""
                  , code: do d_button <- button anil $ text "Click Me!"
                             s_clicks <- s_use $ FRP.s_reduce_e (\agg _ -> agg + 1) 0 $ onClick d_button
                             s_bindDOM_ s_clicks \clicks -> do
                               text $ "You clicked " <> (show clicks) <> " times"
                  , js: """
const ui6 = (I) => {
	const d_button = I.button({}, (I) => I.text("Click Me!"));
	const s_clicks = I.s_use(
		(S) => S.reduce_e((agg, onclickevent) => agg + 1, 0, d_button.onClick)
	);
	I.s_bindDOM(s_clicks, (I) => (clicks) => {
		I.span({}, (I) => { I.text(`You clicked ${clicks} times`); });
	});
};
"""
                  , ps: """
ui6 :: forall e c. DOM e c Unit
ui6 = do
	d_button <- button anil $ text "Click Me!"
	s_clicks <- s_use $ FRP.s_reduce_e (\agg _ -> agg + 1) 0 $ onClick d_button
	s_bindDOM_ s_clicks \clicks -> do
		text $ "You clicked " <> (show clicks) <> " times"
"""
                  }
    renderSection { title: Just "e_collect(prop, drawChildrenFromEvent) / e_emit(prop, event)"
                  , preCode: do p_ anil do
                                  span_ (className "pre") $ text "e_collect"
                                  text " creates an event out of all of the "
                                  span_ (className "pre") $ text "event"
                                  text "s that are "
                                  span_ (className "pre") $ text "e_emit"
                                  text "'ed to "
                                  span_ (className "pre") $ text "prop"
                                  text " during the execution of "
                                  span_ (className "pre") $ text "drawChildrenFromEvent"
                                  text ". "
                                  span_ (className "pre") $ text "drawChildrenFromEvent"
                                  text " is ran by preemptively creating that resulting event and supplying it to "
                                  span_ (className "pre") $ text "drawChildrenFromEvent"
                  , postCode: dnil
                  , code: do e_collect p_counter \e_counter -> do
                               s_counter <- s_use $ FRP.s_reduce_e (\agg change -> agg + change) 0 e_counter
                               d_button1 <- button anil $ text "Click for (+1)"
                               e_emit p_counter $ onClick d_button1 <#> const 1
                               s_bindDOM_ s_counter \counter -> do
                                 text $ "Points: " <> (show counter)
                               d_button2 <- button anil $ text "Click for (-1)"
                               e_emit p_counter $ onClick d_button2 <#> const (-1)
                  , js: """
const ui7 = (I) => {
	I.e_collect('counter', (e_counter) => (I) => {
		const s_counter = I.s_use(
			(S) => S.reduce_e((agg, change) => agg + change, 0, e_counter)
		);
		const d_button1 = I.button({}, (I) => (
			I.text('Click for (+1)')
		));
		I.e_emit('counter', frp.fmap(() => 1, d_button1.onClick);
		I.s_bindDOM(s_counter, (counter) => (I) => {
			I.span({}, (I) => (
				I.text(`Points: ${counter}`)
			));
		});
		const d_button2 = I.button({}, (I) => (
			I.text('Click for (-1)')
		));
		I.e_emit('counter', frp.fmap(() => (-1), d_button2.onClick);
	});
};
"""
                  , ps: """
p_counter = SProxy :: SProxy "counter"

ui7 :: forall e c. DOM e c Unit
ui7 = do
	e_collect p_counter \e_counter -> do
		s_counter <- s_use $ FRP.s_reduce_e (\agg change -> agg + change) 0 e_counter
		d_button1 <- button anil $ text "Click for (+1)"
		e_emit p_counter $ onClick d_button1 <#> const 1
		s_bindDOM_ s_counter \counter -> do
			text $ "Points: " <> (show counter)
		d_button2 <- button anil $ text "Click for (-1)"
		e_emit p_counter $ onClick d_button2 <#> const (-1)
"""
                  }
    renderSection { title: Just "e_collectAndReduce(prop, reducer, init, drawChildren)"
                  , preCode: do p_ anil do
                                  span_ (className "pre") $ text "e_collectAndReduce"
                                  text " is a helper that uses an "
                                  span_ (className "pre") $ text "e_collect"
                                  text " call, reducers the event to a signal based on "
                                  span_ (className "pre") $ text "reducer"
                                  text " and "
                                  span_ (className "pre") $ text "init"
                                  text " and then uses "
                                  span_ (className "pre") $ text "upsertEnv"
                                  text " to stick that signal into the environment at "
                                  span_ (className "pre") $ text "prop"
                                  text ". Compare this example to the previous:"
                  , postCode: do p_ anil do
                                   text "In this example we have split up the rendering into a few subcomponents. Notice that the "
                                   span_ (className "pre") $ text "changeScoreButton"
                                   text " can be used anywhere that there is some "
                                   span_ (className "pre") $ text "e_collect"
                                   text " above it listening to "
                                   span_ (className "pre") $ text "Int"
                                   text " events on "
                                   span_ (className "pre") $ text "counter"
                                   text "."
                                 p_ anil do
                                   text "If you flip to the purescript version of the code you can see that we can actually encode and enforce this constraint in the type system. The return type of the function "
                                   span_ (className "pre") $ text "changeScoreButton"
                                   text " is "
                                   span_ (className "pre") $ text "forall e c. DOM e { counter :: Collector Int | c } Unit"
                                   text ". The "
                                   span_ (className "pre") $ text "DOM e c a"
                                   text " type has 3 type variables (e, c, and a) which help to define it and constrain where the component can be used. "
                                   span_ (className "pre") $ text "e"
                                   text " represents the environment and specifies what types must have been inserted into the environment via "
                                   span_ (className "pre") $ text "upsertEnv"
                                   text " before this component can be used. "
                                   span_ (className "pre") $ text "c"
                                   text " represents the active listening event collectors which specifies what events must be being listened to via "
                                   span_ (className "pre") $ text "e_collect"
                                   text " before this component can be used. And finally "
                                   span_ (className "pre") $ text "a"
                                   text " represents the type of the value returned from the component itself. So in the case of "
                                   span_ (className "pre") $ text "changeScoreButton"
                                   text " we see that it can be used anywhere where something above it is ready to accept the emiting of "
                                   span_ (className "pre") $ text "Int"
                                   text "s at "
                                   span_ (className "pre") $ text "counter"
                                   text ". This means that if we tried to call "
                                   span_ (className "pre") $ text "changeScoreButton"
                                   text " from somewhere else in our code that is not under the umbrella of this "
                                   span_ (className "pre") $ text "e_collectAndReduce"
                                   text " it would actually be a compile time type error."
                  , code: do e_collect p_counter \e_counter -> do
                               s_counter <- s_use $ FRP.s_reduce_e (\agg change -> agg + change) 0 e_counter
                               d_button1 <- button anil $ text "Click for (+1)"
                               e_emit p_counter $ onClick d_button1 <#> const 1
                               s_bindDOM_ s_counter \counter -> do
                                 text $ "Points: " <> (show counter)
                               d_button2 <- button anil $ text "Click for (-1)"
                               e_emit p_counter $ onClick d_button2 <#> const (-1)
                  , js: """
const changeScoreButton = (change, message) => (I) => {
	const d_button = I.button({}, (I) => (
		I.text(message)
	));
	I.e_emit('counter', frp.fmap(() => change, d_button.onClick);
};
const displayScore = (I) => {
	const s_count = I.getEnv('counter');
	I.s_bindDOM(s_counter, (counter) => (I) => {
		I.span({}, (I) => (
			I.text(`Points: ${counter}`)
		));
	});
};
const ui8 = (I) => {
	I.e_collectAndReduce('counter',(agg, change) => agg + change, 0, (I) => {
		changeScoreButton(  1 , 'Click for (+1)')(I);
		displayScore(I);
		changeScoreButton((-1), 'Click for (-1)')(I);
	});
};
"""
                  , ps: """
p_counter = SProxy :: SProxy "counter"

changeScoreButton :: forall e c. Int -> String -> DOM e { counter :: Collector Int | c } Unit
changeScoreButton change message = do
	d_button <- button anil $ text message
	e_emit p_counter $ onClick d_button <#> const change

displayScore :: forall e c. DOM { counter :: FRP.Signal Int | e } c Unit
displayScore = do
	s_counter <- getEnv p_counter
	s_bindDOM_ s_counter \counter -> do
		text $ "Points: " <> (show counter)

ui8 :: forall e c. DOM e c Unit
ui8 = do
	e_collectAndReduce p_counter (\agg change -> agg + change) 0 do
		changeScoreButton   1  "Click for (+1)"
		displayScore
		changeScoreButton (-1) "Click for (-1)"
"""
                  }
    renderSection { title: Just "d_stash(drawChildren) => ImpulseStash / d_apply(impulseStash)"
                  , preCode: dnil
                  , postCode: p_ anil do
                                span_ (className "pre") $ text "d_stash"
                                text $ " will store the rendered content in a value that can be rendered in a different location in DOM via "
                                span_ (className "pre") $ text "d_apply"
                                text $ ". This is especially useful when you need to render content in a place that doesn't have all of the necessary context needed to calculate the UI. For example if you want to trigger a notification from a place deep within the UI with content that depends on state that is local to that area of the UI, but need the notification DOM element to exist higher in the DOM tree, you can render it within a stash from the place with all the context, and bubble up the resulting notification to be applied at the point in DOM that makes the most sense."
                  , code: do ul_ anil do
                               stash <- d_stash do
                                 li_ anil $ text "out"
                                 li_ anil $ text "of"
                                 li_ anil $ text "order?"
                               li_ anil $ text "You"
                               li_ anil $ text "thought"
                               li_ anil $ text "this"
                               li_ anil $ text "was"
                               d_apply stash
                  , js: """
const ui9 = (I) => {
	I.ul({}, (I) => {
		const stash = I.d_stash((I) => {
			I.li({}, (I) => I.text("out"));
			I.li({}, (I) => I.text("of"));
			I.li({}, (I) => I.text("order?"));
		});
		I.li({}, (I) => I.text("You"));
		I.li({}, (I) => I.text("thought"));
		I.li({}, (I) => I.text("this"));
		I.li({}, (I) => I.text("was"));
		I.d_apply(stash);
	});
};
"""
                  , ps: """
ui9 :: forall e c. DOM e c Unit
ui9 = do
	ul_ anil do
		stash <- d_stash do
			li_ anil $ text "out"
			li_ anil $ text "of"
			li_ anil $ text "order?"
		li_ anil $ text "You"
		li_ anil $ text "thought"
		li_ anil $ text "this"
		li_ anil $ text "was"
		d_apply stash
"""
                  }
    div_ (className "mini-spacer") dnil
    p_ anil do
      text "This guide is a very early work in progress, if you would like to learn more I'd recommend checking out the project on "
      a_ (href "https://github.com/mitchdzugan/purescript-impulse") $ text "GitHub"
      text " and filing an issue if you would like to see anything else explained. I'm juggling a lot of different aspects of this project at the moment and likely won't be making too many more updates to the guides in the near future unless other people express interest, because as of right now I'm just rambling to myself :P"
    div_ (className "spacer") dnil

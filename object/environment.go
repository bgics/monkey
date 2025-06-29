package object

type Environment struct {
	store map[string]Object
	outer *Environment
}

func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer
	return env
}

func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s, outer: nil}
}

func (e *Environment) Get(name string) (Object, bool) {
	if obj, ok := e.store[name]; !ok && e.outer != nil {
		return e.outer.Get(name)
	} else {
		return obj, ok
	}
}

func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val
	return val
}

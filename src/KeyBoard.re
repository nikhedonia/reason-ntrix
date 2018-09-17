let register = [%bs.raw {|cb => {  
  window.addEventListener('keydown', e=>cb(e.key))
}|}];

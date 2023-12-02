layout asm
layout regs
focus cmd

display (char)$rax

break read_character_with_lookahead.debug1
break read_character_with_lookahead.debug2
break reset_lookahead_buffers.debug3

display (long)lookahead_buffer_size
display (char*)lookahead_buffer
display (long)next_lookahead_buffer_size
display (char*)next_lookahead_buffer
display (long)lookahead_index

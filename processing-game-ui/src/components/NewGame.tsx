import { useState } from 'react';
import { Button, Dialog, DialogActions, DialogContent, DialogTitle, TextField } from '@mui/material';

interface NewGameProps {
    onNewGame: (programs: number, processors: number, ados: number) => void;
}

export const NewGame: React.FC<NewGameProps> = ({ onNewGame }) => {
    const [open, setOpen] = useState(false);
    const [programs, setPrograms] = useState(3);
    const [processors, setProcessors] = useState(5);
    const [ados, setAdos] = useState(3);

    const handleClickOpen = () => {
        setOpen(true);
    };

    const handleClose = () => {
        setOpen(false);
    };

    const handleCreate = () => {
        onNewGame(programs, processors, ados);
        setOpen(false);
    };

    return (
        <div>
            <Button variant="outlined" onClick={handleClickOpen}>
                New Game
            </Button>
            <Dialog open={open} onClose={handleClose}>
                <DialogTitle>Create New Game</DialogTitle>
                <DialogContent>
                    <TextField
                        autoFocus
                        margin="dense"
                        label="Number of Programs"
                        type="number"
                        fullWidth
                        value={programs}
                        onChange={(e) => setPrograms(parseInt(e.target.value, 10))}
                    />
                    <TextField
                        margin="dense"
                        label="Number of Processors"
                        type="number"
                        fullWidth
                        value={processors}
                        onChange={(e) => setProcessors(parseInt(e.target.value, 10))}
                    />
                    <TextField
                        margin="dense"
                        label="Number of ADOs per program"
                        type="number"
                        fullWidth
                        value={ados}
                        onChange={(e) => setAdos(parseInt(e.target.value, 10))}
                    />
                </DialogContent>
                <DialogActions>
                    <Button onClick={handleClose}>Cancel</Button>
                    <Button onClick={handleCreate}>Create</Button>
                </DialogActions>
            </Dialog>
        </div>
    );
};

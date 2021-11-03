import dgl
import dgllife.model
import torch
import torch.utils.data

import oscml.models.model_gnn
import oscml.models.model_bilstm


class BiLstmForPceTransfer(oscml.models.model_bilstm.BiLstmForPce):

    def forward(self, index_sequences):
        if self.trainer.current_epoch < 400:
            with torch.no_grad():
                x= self.embedding(index_sequences)
                h, _ = self.bilstm(x)
                attention_value_msum = self.attention(h)
        else:
            x = self.embedding(index_sequences)
            h, _ = self.bilstm(x)
            attention_value_msum = self.attention(h)

        o = self.mlp(attention_value_msum)
        o = o.view(len(o))
        return o


class SimpleGNNTransfer(oscml.models.model_gnn.SimpleGNN):

    def forward(self, graphs):
        if isinstance(graphs, list):
            g_batch = dgl.batch(graphs)
        else:
            g_batch = graphs

        seq_types = g_batch.ndata['type']
        if self.padding_index is not None:
            minus_one = self.one.to(self.device).expand(seq_types.size())
            seq_types = seq_types + minus_one

        if self.trainer.current_epoch < 400:
            with torch.no_grad():
                h = self.embedding(seq_types)
                for layer in self.conv_modules:
                    h = layer(g_batch, h)
                g_batch.ndata['h'] = h
        else:
            h = self.embedding(seq_types)
            for layer in self.conv_modules:
                h = layer(g_batch, h)
            g_batch.ndata['h'] = h

        mean = dgl.mean_nodes(g_batch, 'h')
        o = self.mlp(mean)
        o = o.view(len(o))
        return o


class AttentiveFPTransfer(dgllife.model.AttentiveFPPredictor):

    def forward(self, g, node_feats, edge_feats, get_node_weight=False):
        with torch.no_grad():
            node_feats = self.gnn(g, node_feats, edge_feats)

        if get_node_weight:
            g_feats, node_weights = self.readout(g, node_feats, get_node_weight)
            return self.predict(g_feats), node_weights
        else:
            g_feats = self.readout(g, node_feats, get_node_weight)
            return self.predict(g_feats)
